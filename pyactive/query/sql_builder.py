#coding:utf8
from ..utils import is_array, is_hash
from .join_clause import JoinClause
import re


class SQLBuilder(object):
    
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

    def new_instance(self):
        return self.__class__()

    def first(self):
        tmp_limit, tmp_offset = self._limit, self._offset
        self.limit(1).offset(0)
        rows = self.all()
        self._limit, self._offset = tmp_limit, tmp_offset
        return rows[0] if rows else None
    
    def all(self):
        sql, params = self.to_sql()
        rows = self.conn.fetch_all(sql, *params)
        return rows

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
    
    def distinct(self):
        self._distinct = True
        return self

    def to_sql(self):
        sqls, params = [], []
        if self._distinct:
            sqls.append( 'SELECT DISTINCT %s FROM `%s`' % (self._compile_select(), self._from) )
        else:
            sqls.append( 'SELECT %s FROM `%s`' % (self._compile_select(), self._from) )
        
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

    def _compile_select(self):
        selects = self._selects
        new_selects = []
        for select in selects:
            select_lower = select.lower()
            if select == '*':
                new_selects.append(select)
            elif ' as ' in select_lower: # aaa.bbb as a.b, ccc.ddd as c.d
                new_selects.append(
                    ', '.join([ 
                        ' AS '.join([ 
                            '.'.join([ 
                                '`%s`' % part for part in parts.split('.') # [aaa, bbb]
                            ]) for parts in segment.strip().split(' as ') # [ aaa.bbb, a.b ]
                        ]) for segment in select_lower.split(',') # [ aaa.bbb as a.b, ccc.ddd as c.d ]
                    ])
                )
            else:
                new_selects.append(self._compile_fieldname(select))
        return ', '.join(new_selects)
    
    def _compile_fieldname(self, name):
        return '`%s`.`%s`' % (self._from, name) if name else name
    
    def _postion_flag(self, list_or_set):
        return ', '.join(self.__class__.POSITION_FLAG * len(list_or_set))
    
    def _compile_where_or_having(self, conditions):
        sqls, params = [], []
        for condition in conditions:
            if is_array(condition):
                if len(condition) == 1: # 只有一个，表示是一个不带参数的sql
                    sqls.append(condition[0])
                else:
                    sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition): # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if is_array(v):
                        if v: # 数组里面有元素
                            sqls.append('%s IN (%s)' % (self._compile_fieldname(k), self._postion_flag(v)))
                            params.extend(list(v)) # 如果是set，就必须转一下
                    elif v is None:
                        sqls.append('%s IS NULL' % self._compile_fieldname(k))
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
