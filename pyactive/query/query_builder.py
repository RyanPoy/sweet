# -*- coding:utf-8 -*-
from ..utils import is_array, is_hash

class QueryBuilder(object):
    
    POSITION_FLAG = '?'
    
    def __init__(self):
        self._selects = []
        self._wheres = []
        self._distinct = False
        self._from = None
        self._groups = []

    def group_by(self, *args):
        for arg in args:
            self._groups += [ x.strip() for x in arg.split(',') if x and x.strip() ]
        return self

    def where(self, *args, **kwargs):
        if args:
            self._wheres.append(args)
        if kwargs:
            self._wheres.append(kwargs)
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
        
        where_sql, where_params = self._compile_where()
        if where_sql:
            sqls.append('WHERE %s' % where_sql)
            params += where_params
            
        group_by_sql = self._compile_group()
        if group_by_sql:
            sqls.append('GROUP BY %s' % group_by_sql)

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
                new_selects.append(self._complie_fieldname(select))
        return ', '.join(new_selects)
    
    def _complie_fieldname(self, name):
        return '`%s`.`%s`' % (self._from, name)
    
    def _postion_flag(self, list_or_set):
        return ', '.join(self.__class__.POSITION_FLAG * len(list_or_set))

    def _compile_where(self):
        sqls, params = [], []
        for where in self._wheres:
            if is_array(where):
                if len(where) == 1: # 只有一个，表示是一个不带参数的sql
                    sqls.append(where[0])
                else:
                    sqls.append(where[0])
                    params.extend(where[1:])
            elif is_hash(where): # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in where.iteritems():
                    if is_array(v):
                        if v: # 数组里面有元素
                            sqls.append('%s IN (%s)' % (self._complie_fieldname(k), self._postion_flag(v)))
                            params.extend(list(v)) # 如果是set，就必须转一下
                    elif v is None:
                        sqls.append('%s IS NULL' % self._complie_fieldname(k))
                    else:
                        sqls.append('%s = %s' % (self._complie_fieldname(k), self.__class__.POSITION_FLAG))
                        params.append(v)
        sql = ' AND '.join(sqls)
        return sql, params
    
    def _compile_group(self):
        return ', '.join([ self._complie_fieldname(group) for group in self._groups ])
#         if new_groups:
#             sql = '%s GROUP BY %s' % (sql, ','.join(new_groups))
#             having_sql, having_params = self._having_chain.compile()
#             if having_sql:
#                 sql = '%s %s' % (sql, having_sql)
#             if having_params:
#                 params.extend(having_params)