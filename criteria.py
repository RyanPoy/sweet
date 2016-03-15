# coding:utf8
from sweet.utils import is_array, is_hash, flatten
import copy
import re


class Criteria(object):

    POSITION_FLAG = '?'

    def __init__(self, conn=None):
        self._selects = []
        self._wheres = []
        self._havings = []
        self._distinct = False
        self._from = None
        self._groups = []
        self._orders = []
        self._limit = None
        self._offset = 0
        self._joins = []
        self.conn = conn
        self._aggregate = None
        self.__record_class = None

    def new_instance(self):
        instance = self.__class__()
        instance.from_(self._from)
        return instance

    def delete(self):
        sqls, params = [], []
        sqls.append('DELETE FROM `%s`' % self._from)

        join_sql, join_params = self._compile_join()
        if join_sql:
            sqls.append(' %s' % join_sql)
            params += join_params

        where_sql, where_params = self._compile_where_or_having(self._wheres)
        if where_sql:
            sqls.append(' WHERE %s' % where_sql)
            params += where_params
        return self.conn.execute(''.join(sqls), *params)

    def update(self, dict_values={}, **kwargs):
        field_value_dict = {}
        field_value_dict.update(dict_values)
        field_value_dict.update(kwargs)
        sqls, params = [], []
        sqls.append('UPDATE `%s`' % self._from)

        join_sql, join_params = self._compile_join()
        if join_sql:
            sqls.append(' %s' % join_sql)
            params += join_params

        fields_sql = []
        for k, v in field_value_dict.iteritems():
            fields_sql.append('%s = ?' % self._compile_fieldname(k))
            params.append(v)
        sqls.append(' SET %s' % ', '.join(fields_sql))

        where_sql, where_params = self._compile_where_or_having(self._wheres)
        if where_sql:
            sqls.append(' WHERE %s' % where_sql)
            params += where_params
        return self.conn.execute(''.join(sqls), *params)

    def insert(self, multiple_value_list=None, **kwargs):
        if multiple_value_list:
            return self._batch_insert(multiple_value_list)
        return self._single_insert(**kwargs)

    def _single_insert(self, **kwargs):
        columns, values = [], []
        for k, v in kwargs.iteritems():
            columns.append(self._compile_fieldname(k, False))
            values.append(v)
        sql = 'INSERT INTO `%s` (%s) VALUES (%s)' % (self._from, ', '.join(columns), self._postion_flag(values))
        return self.conn.execute_lastrowid(sql, *values)

    def _batch_insert(self, multiple_value_list):
        columns, values_list, set_columns = [], [], False
        for values_dict in multiple_value_list:
            values = []
            for k, v in values_dict.iteritems():
                if set_columns is False:
                    columns.append(self._compile_fieldname(k, False))
                values.append(v)
            set_columns = True
            values_list.append(values)
        sql = 'INSERT INTO `%s` (%s) VALUES %s' % (self._from, ', '.join(columns), ', '.join([ '(%s)' % self._postion_flag(vs) for vs in values_list ]))
        return self.conn.execute_rowcount(sql, *flatten(values_list))

    def first(self):
        tmp_limit = self._limit
        self.limit(1)
        rows = self.all()
        self._limit = tmp_limit
        return rows[0] if rows else None

    def last(self):
        cnt = self.count()
        if not cnt: return None
        tmp_limit, tmp_offset = self._limit, self._offset
        self.limit(1).offset(cnt - 1)
        rows = self.all()
        self._limit, self._offset = tmp_limit, tmp_offset
        return rows[0] if rows else None

    def all(self):
        sql, params = self.to_sql()
        records = self.conn.fetchall(sql, *params)
        if self.record_class and self._aggregate is None: # 表示是需要转化为对象返回的，而不是一个list（list的元素是dict）
            return [ self.record_class(r) for r in records ]
        return records

    def join(self, tablename, *args):
        if isinstance(tablename, JoinClause):
            self._joins.append(tablename)
        else:
            self._joins.append(JoinClause(tablename).on(*args))
        return self

    def left_join(self, tablename, *args):
        if isinstance(tablename, JoinClause):
            self._joins.append(tablename)
        else:
            self._joins.append(JoinClause(tablename, 'LEFT').on(*args))
        return self

    def right_join(self, tablename, *args):
        if isinstance(tablename, JoinClause):
            self._joins.append(tablename)
        else:
            self._joins.append(JoinClause(tablename, 'RIGHT').on(*args))
        return self

    def page(self, page_num, limit):
        self._limit = limit
        self._offset = page_num * limit
        return self

    def limit(self, limit):
        self._limit = limit
        return self

    def offset(self, offset):
        self._offset = offset
        return self

    def group_by(self, *args):
        for arg in args:
            self._groups += [ x.strip() for x in arg.split(',') if x and x.strip() ]
        return self

    def order_by(self, *args):
        for arg in args:
            for x in arg.split(','):
                if x and x.strip():
                    x = re.sub('( desc| asc)( |$)', lambda m: '%s%s' % (m.group(1).upper(), m.group(2)), x, re.I)
                    self._orders.append(x)
        return self

    def where(self, *args, **kwargs):
        if args:
            self._wheres.append(args)
        if kwargs:
            self._wheres.append(kwargs)
        return self

    def having(self, *args, **kwargs):
        if args:
            self._havings.append(args)
        if kwargs:
            self._havings.append(kwargs)
        return self

    def select(self, *select_columns):
        self._selects += select_columns
        return self

    def from_(self, tablename):
        self._from = tablename
        return self

    @property
    def record_class(self):
        return self.__record_class

    def set_record_class(self, record_class):
        self.__record_class = record_class
        self.from_(self.__record_class.table_name)
        return self

    def distinct(self):
        self._distinct = True
        return self

    def count(self, column = '*'):
        return self._aggregate_func('COUNT(%s)' % column)

    def sum(self, column):
        return self._aggregate_func('SUM(%s)' % column)

    def max(self, column):
        return self._aggregate_func('MAX(%s)' % column)

    def min(self, column):
        return self._aggregate_func('MIN(%s)' % column)

    def avg(self, column):
        return self._aggregate_func('AVG(%s)' % column)

    def _aggregate_func(self, aggeregate):
        self._aggregate = aggeregate
        row = self.first()
#         rows = self.all()
#         if not rows: return 0
#         row = rows[0]
        v = row['aggregate'] if row else 0
        self._aggregate = None
        return v

    def to_sql(self):
        sqls, params = [], []
        aggregate_or_select_sql = self._compile_aggregate() or self._compile_select()
        if self._distinct:
            sqls.append('SELECT DISTINCT %s FROM `%s`' % (aggregate_or_select_sql, self._from))
        else:
            sqls.append('SELECT %s FROM `%s`' % (aggregate_or_select_sql, self._from))

        join_sql, join_params = self._compile_join()
        if join_sql:
            sqls.append(join_sql)
            params += join_params

        where_sql, where_params = self._compile_where_or_having(self._wheres)
        if where_sql:
            sqls.append('WHERE %s' % where_sql)
            params += where_params

        group_by_sql = self._compile_group()
        if group_by_sql:
            sqls.append('GROUP BY %s' % group_by_sql)

        having_sql, having_params = self._compile_where_or_having(self._havings)
        if having_sql:
            sqls.append('HAVING %s' % having_sql)
            params += having_params

        order_by_sql = self._compile_order()
        if order_by_sql:
            sqls.append('ORDER BY %s' % order_by_sql)

        limit_offset_sql = self._compile_limit_and_offset()
        if limit_offset_sql:
            sqls.append(limit_offset_sql)

        return ' '.join(sqls), params

    def _compile_aggregate(self):
        return '%s AS aggregate' % self._aggregate if self._aggregate else ''

    def _compile_select(self):
        selects = self._selects or ['*']
        new_selects = []
        for select in selects:
            select_lower = select.lower()
            if select == '*':
                new_selects.append(select)
            elif ' as ' in select_lower:  # aaa.bbb as a.b, ccc.ddd as c.d
                new_selects.append(
                    ', '.join([
                        ' AS '.join([
                            '.'.join([
                                '`%s`' % part for part in parts.split('.')  # [aaa, bbb]
                            ]) for parts in segment.strip().split(' as ')  # [ aaa.bbb, a.b ]
                        ]) for segment in select_lower.split(',')  # [ aaa.bbb as a.b, ccc.ddd as c.d ]
                    ])
                )
            else:
                new_selects.append(self._compile_fieldname(select))
        return ', '.join(new_selects)

    def _compile_fieldname(self, name, prefix_table = True):
        if prefix_table:
            return '`%s`.`%s`' % (self._from, name) if name else name
        return '`%s`' % name if name else name

    def _postion_flag(self, list_or_set):
        return ', '.join(self.__class__.POSITION_FLAG * len(list_or_set))

    def _compile_where_or_having(self, conditions):
        sqls, params = [], []
        for condition in conditions:
            if is_array(condition):
                condition_len = len(condition)
                if condition_len == 1:  # 只有一个，表示是一个不带参数的sql
                    sqls.append(condition[0])
                elif condition_len == 2 and isinstance(condition[1], Criteria):  # 又是一个criteria
                    sub_criteria_sql, sub_criteria_params = condition[1].to_sql()
                    sqls.append('%s (%s)' % (condition[0], sub_criteria_sql))
                    params += sub_criteria_params
                else:
                    sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition):  # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if v is None:
                        sqls.append('%s IS NULL' % self._compile_fieldname(k))
                    elif isinstance(v, Criteria):  # 又是一个criteria
                        sub_criteria_sql, sub_criteria_params = v.to_sql()
                        sqls.append('%s = (%s)' % (self._compile_fieldname(k), sub_criteria_sql))
                        params += sub_criteria_params
                    elif is_array(v):
                        if v:  # 数组里面有元素
                            sqls.append('%s IN (%s)' % (self._compile_fieldname(k), self._postion_flag(v)))
                            params.extend(list(v))  # 如果是set，就必须转一下
                    else:
                        sqls.append('%s = %s' % (self._compile_fieldname(k), self.__class__.POSITION_FLAG))
                        params.append(v)
        sql = ' AND '.join(sqls)
        return sql, params

    def _compile_group(self):
        return ', '.join([ self._compile_fieldname(group) for group in self._groups ])

    def _compile_order(self):
        sqls = []
        for order in self._orders:
            flag = ' '
            if ' DESC' in order: flag = ' DESC'
            elif ' ASC' in order: flag = ' ASC'
            sqls.append(flag.join([ self._compile_fieldname(o.strip()) for o in order.split(flag) ]))
        return ', '.join(sqls)

    def _compile_limit_and_offset(self):
        sqls = []
        if self._limit is not None:
            sqls.append('LIMIT %s' % self._limit)
        if self._offset:
            sqls.append('OFFSET %s' % self._offset)
        return ' '.join(sqls)

    def _compile_join(self):
        sqls, params = [], []
        for join in self._joins:
            join_sql, join_params = join.to_sql()
            if join_sql:
                sqls.append(join_sql)
                params += join_params
        return ' '.join(sqls), params


    ########################
    # PYTHON MAGIC METHODS #
    ########################
    def __deepcopy__(self, memo):
        """ Deep copy """
        obj = self.__class__()
        for k, v in self.__dict__.iteritems():
            if k == 'conn': # conn can not deep copy
                obj.__dict__[k] = v
            else:
                obj.__dict__[k] = copy.deepcopy(v, memo)
        return obj

    def __repr__(self):
        new_criteria = copy.deepcopy(self)
        cnt = new_criteria.count()
        if cnt > 3:
            new_criteria.limit(3)
        data = new_criteria.all()
        if cnt > 3:
            data.append('...(remaining elements truncated)...')
        return repr(data)

    def __len__(self):
        return self.count()

    def __iter__(self):
        return iter(self.count())

    def __getitem__(self, item):
        new_criteria = copy.deepcopy(self)
#         new_criteria = self
        cnt = new_criteria.count()
        if cnt > item + 1:
            return None
        return new_criteria.limit(1).offset(item+1)


########## join clause ##########
class JoinClause(object):

    def __init__(self, tablename, direction = 'INNER'):
        self.tablename = tablename
        self.direction = direction
        self.ons = []

    def on(self, *args, **kwargs):
        if args:
            self.ons.append(args)
        if kwargs:
            self.ons.append(kwargs)
        return self

    def to_sql(self):
        sqls, params = ['%s JOIN `%s`' % (self.direction, self.tablename)], []
        on_sql, on_params = self._compile_on()
        if on_sql:
            sqls.append('ON %s' % on_sql)
            params += on_params
        return ' '.join(sqls), params

    def _compile_on(self):
        sqls, params = [], []
        for condition in self.ons:
            if is_array(condition):
                if len(condition) == 1:  # 只有一个，表示是一个不带参数的sql
                    sqls.append(condition[0])
                else:
                    sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition):  # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if is_array(v):
                        if v:  # 数组里面有元素
                            sqls.append('%s IN (%s)' % (self._compile_fieldname(k), self._postion_flag(v)))
                            params.extend(list(v))  # 如果是set，就必须转一下
                    elif v is None:
                        sqls.append('%s IS NULL' % self._compile_fieldname(k))
                    else:
                        sqls.append('%s = %s' % (self._compile_fieldname(k), self.__class__.POSITION_FLAG))
                        params.append(v)
        sql = ' AND '.join(sqls)
        return sql, params
