#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from pyrails.activesupport import RecordNotFound, Inflection, is_array, is_str, is_hash, flatten, to_i


class Collection(object):
    
    DELETE_STR, UPDATE_STR = 'DELETE', 'UPDATE'

    def __init__(self, model_class):
        self._deleteall_or_updateall = None
        self._update_attrs_dict = {}
        self._selects       = []
        self._wheres        = []
        self._limit         = None
        self._offset        = None
        self._orders        = []
        self._groups        = []
        self._havings       = []
        self._joins         = []
        # self._join_table_list = []
        self._func          = None
        self._model_class   = model_class
        self._table_name    = self._model_class.table_name
        self._db            = self._model_class._get_db()
        self.__fk_value     = {}

    @property    
    def all(self):
        sql, params = self.delete_or_update_or_find_sql()
        rows = self._db.fetchall(sql, params)
        return [ self._model_class(**field_name_value_dict_row) for field_name_value_dict_row in rows ]
    
    @property
    def first(self):
        sql, params = self.limit(1, 0).delete_or_update_or_find_sql()
        row = self._db.fetchone(sql, params)
        return self._model_class(**row) if row else None

    @property
    def last(self):
        count = self.count()
        self._func = None
        if count < 0:
            return None
        sql, params = self.limit(1, count - 1).delete_or_update_or_find_sql()
        row = self._db.fetchone(sql, params)
        return self._model_class(**row) if row else None
    
    def save(self, model):
        sql = 'INSERT INTO %s (%s) VALUES (%s)' % (model.table_name_sql, model.column_names_sql(), model.column_placeholder_sql)
        params = [ getattr(model, c, None) for c in model.column_names ]
        model.id = self._db.execute_lastrowid(sql, params)
        return model.id

    def delete_all(self):
        self._deleteall_or_updateall = self.DELETE_STR
        self._update_attr_dict = {}
        sql, params = self.delete_or_update_or_find_sql()
        return self._db.execute_rowcount(sql, params)
    
    def update_all(self, **kw_attrs):
        self._deleteall_or_updateall = self.UPDATE_STR
        self._update_attrs_dict.update(**kw_attrs)
        sql, params = self.delete_or_update_or_find_sql()        
        return self._db.execute_rowcount(sql, params)

    def find(self, *ids):
        records = self.where(id=ids).all
        not_repeat_ids = set(ids)
        if len(records) != len(not_repeat_ids):
            raise RecordNotFound()
        return records[0] if len(not_repeat_ids) == 1 else records

    def count(self, column_name='*'):
        return self.__func('COUNT')
    
    def sum(self, column_name):
        return self.__func('SUM', column_name)
    
    def __func(self, func_name, func_value='*'):
        self._func = (func_name, func_value)
        sql, params = self.delete_or_update_or_find_sql()
        row = self._db.fetchone(sql, params)
        self._func = None
        return row.values()[0] if row else 0
    
    def limit(self, limit, offset=0):
        self._limit, self._offset = limit, offset
        return self

    def select(self, *args):
        for arg in args:
            self._selects.append(arg)
        return self
        
    def where(self, *sql_and_params, **conditions):
        return self.__where_or_having(self._wheres, *sql_and_params, **conditions)

    def order(self, order):
        if order:
            self._orders.append(order) 
        return self

    def group(self, group):
        if group:
            self._groups.append(group)
        return self

    def having(self, *sql_and_params, **conditions):
        return self.__where_or_having(self._havings, *sql_and_params, **conditions)
    
    def joins(self, *joins):
        for join in joins:
            self._joins.append(join)
        return self
    
    def __where_or_having(self, codition_collection, *sql_and_params, **conditions):
        if sql_and_params:
            codition_collection.append(sql_and_params)
        elif conditions:
#            for key in condtions.keys(): # must use keys, not iterkeys.
#                if type(key) is self._model_class.__metaclass__:
#                if type(key) is self._model_class:
#                    new_key = '%s_id' % Inflection.hungarian_of(key.__class__.__name__)
#                    condtions[new_key] = key.id
#                    condtions.pop(key)
            codition_collection.append(conditions)
        return self
            
    def delete_or_update_or_find_sql(self):
        params  = []

        sql = self.__add_delete_or_update_or_select_or_function(params)
        sql = self.__add_joins(sql, self._joins)
        sql = self.__add_wheres(sql, params, self._wheres)
        
        sql = self.__add_group_having(sql, self._groups, self._havings, params)
        sql = self.__add_order(sql, self._orders)
        sql = self.__add_limit_offset(sql, self._limit, self._offset)
        return sql, params

    def __add_delete_or_update_or_select_or_function(self, params):
        if self._deleteall_or_updateall == self.DELETE_STR:
            return 'DELETE FROM %s' % self._table_name
        elif self._deleteall_or_updateall == self.UPDATE_STR:
            columns_sql = ', '.join([ '`%s` = ?' % key for key in self._update_attrs_dict.iterkeys() ])
            params.extend([ v for _, v in self._update_attrs_dict.iteritems() ])
            return 'UPDATE %s SET %s' % (self._table_name, columns_sql)
        elif self._func:
            return 'SELECT %s(%s) AS %s FROM %s' % (self._func[0].upper(), self._func[1], self._func[0].lower(), self._table_name)
        elif self._selects:
            select_sql = ', '.join([ '%s.%s' % (self._table_name, s) for s in flatten(self._selects) ])
            return 'SELECT %s FROM %s' % (select_sql, self._table_name)
        # elif self._joins:
        #     pass
        else:
            return 'SELECT %s.* FROM %s' % (self._table_name, self._table_name)
            
    
    def __add_limit_offset(self, sql, limit, offset):
        limit, offset = to_i(limit), to_i(offset)
        if limit:
            sql += ' LIMIT %s' % limit
            if offset:
                sql += ' OFFSET %s' % offset
        return sql
    
    def __add_group_having(self, sql, groups, havings, params):
        new_groups = []
        for group in groups:
            if group and group.strip():
                new_groups.extend(group.split(','))
        
        new_groups = [ '%s.%s' % (self._table_name, group.strip()) 
                        for group in new_groups if group and group.strip() ]
        if new_groups:
            sql = '%s GROUP BY %s' % (sql, ','.join(new_groups))
            sql = self.__add_conditions_of(sql, params, havings, 'HAVING')
        
        return sql
    
    def __add_wheres(self, sql, params, wheres):
        if self._joins:
            return self.__add_conditions_of(sql, params, wheres, 'AND')
        else:
            return self.__add_conditions_of(sql, params, wheres, 'WHERE')
    
    def __add_conditions_of(self, sql, params, conditions, where_or_having='WHERE'):
        condition_sqls = []
        for condition in conditions:
            if is_array(condition):
                if len(condition) == 1: # 只有一个，表示是一个不带参数的sql
                    condition_sqls.append(condition[0])
                else:
                    condition_sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition): # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if is_array(v):
                        interrogations = ', '.join(['?'] * len(v))
                        condition_sqls.append('%s.%s in (%s)' % (self._table_name, k, interrogations))
                        params.extend(list(v)) # 如果是set，就必须转一下
                    else:
                        condition_sqls.append('%s.%s = ?' % (self._table_name, k))
                        params.append(v)
        if condition_sqls:
            sql = '%s %s %s' % (sql, where_or_having, ' AND '.join(condition_sqls))
        return sql
    
    def __add_order(self, sql, orders):
        order_str = ', '.join([ order for order in orders if order ])
        return '%s ORDER BY %s' % (sql, order_str) if order_str else sql

    def __add_joins(self, sql, joins):
        buff = []
        self.__add_array_joins(buff, self._model_class, joins)
        if buff:
            return '%s %s' % (sql, ' '.join(buff))
        return sql

    def __add_array_joins(self, buff, this_class, joins):
        """
        @param joins: Either an SQL fragment for additional joins like 
                        "LEFT JOIN comments ON comments.post_id = id" (rarely needed)
        """
        for join in joins: 
            # must be a string or list
            if is_str(join):
                self.__add_str_join(buff, this_class, join)
            elif is_hash(join):
                self.__add_hash_joins(buff, this_class, join)

    def __add_hash_joins(self, buff, this_class, joins):
        for k, v in joins.iteritems():
            self.__add_str_join(buff, this_class, k)
            
            k_association = this_class.association_of(k)
            k_class = k_association.target

            if is_str(v):
                self.__add_str_join(buff, k_class, v)
            elif is_array(v):
                self.__add_array_joins(buff, k_class, v)
            elif is_hash(v):
                self.__add_hash_joins(buff, k_class, v)

    def __add_str_join(self, buff, this_class, join):
        association = this_class.association_dict.get(join, None)
        if association: # a association: belongs_to, has_one, has_many
            self.__add_join(buff, association, this_class)
        else:
            buff.append(join)
        return self

    def __add_join(self, buff, association, this_class):
        """ 
        eg.
            __add_association_join(Association.Type.belongs_to, Post, User)
          ==> INNERT JOIN users ON users.id = posts.user_id

            __add_association_join(Association.Type.has_one, User, Post)
          ==> INNERT JOIN posts ON post.user_id = users.id

            __ad__association_join(Association.Type.has_many, User, Post)
          ==> INNERT JOIN posts ON post_user_id = users.id
        """
        target_class = association.target
        if association.is_belongs_to():
            _sql = 'INNER JOIN %s ON %s.id = %s.%s' % (target_class.table_name, target_class.table_name, this_class.table_name, association.foreign_key)
        elif association.is_has_one() or association.is_has_many():
            _sql = 'INNER JOIN %s ON %s.%s = %s.id' % (target_class.table_name, target_class.table_name, association.foreign_key, this_class.table_name)
        # self._join_table_list.append(target_class.table_name)
        if _sql:
            buff.append(_sql)

    def _set_fk_value_for_build_or_create(self, fk_name_and_value_dict):
        self.__fk_value = fk_name_and_value_dict
        return self

    def build(self, **attributes):
        for k, v in self.__fk_value.iteritems():
            if k not in attributes:
                attributes[k] = v
        return self._model_class(**attributes)

    def create(self, **attributes):
        record = self.build(**attributes)
        record.save()
        return record

    def __iter__(self):
        return iter(self.all)

    def __len__(self):
        cnt = self.count()
        self._func = None
        return cnt
    
    def __getitem__(self, index):
        relt = self.limit(index+1, index).all
        if not relt:
            raise IndexError('%s out of collection range')
        self._limit, self._offset = None, None
        return relt[0]

    def __str__(self):
        return self.all
