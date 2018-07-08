#coding: utf8
from sweet.utils import *


class where_expr(object):

    ops = {
        'gt': ('', '>'), 
        'not_gt': ('not', '>'), 
        'gte': ('', '>='), 
        'not_gte': ('not', '>='), 
        'lt': ('', '<'), 
        'not_lt': ('not', '<'), 
        'lte': ('', '<='), 
        'not_lte': ('not', '<='),
        'not': ('not', '!='),
        'bt': ('', 'between'), # between
        'not_bt': ('not', 'between') # not between
    }

    def __init__(self, s):
        vs = s.split('__')
        if vs[-1] in self.ops: # 后缀在 ops里面
            self.is_or_not, self.operator = self.ops[vs[-1]]
            self.column = s[:-len(vs[-1])-2]
        else:
            self.is_or_not, self.column, self.operator = '', s, '='



class SQLBuilder(object):

    def __init__(self):
        self.tbname = ''
        self._select = []
        self._distinct = False
        self._where = []
        self._or = []
        self._where_bindings = []
        self._having = []
        self._or_having = []
        self._having_bindings = []
        self._bindings = []
        self._group_by = []
        self._order_by = []

    def distinct(self):
        self._distinct = True

    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self._select.append(c)
        return self

    @property
    def bindings(self):
        return self._bindings

    def from_(self, tbname):
        self.tbname = tbname
        return self

    def where(self, **kwargs):
        return self.__where_or_having_or(self._where, self._where_bindings, 'AND', **kwargs)

    def or_(self, **kwargs):
        return self.__where_or_having_or(self._where, self._where_bindings, 'OR', **kwargs)

    def having(self, **kwargs):
        return self.__where_or_having_or(self._having, self._having_bindings, 'AND', **kwargs)

    def or_having(self, **kwargs):
        return self.__where_or_having_or(self._having, self._having_bindings, 'OR', **kwargs)

    def __where_or_having_or(self, collection, bindings, and_or, **kwargs):
        for k, v in kwargs.items():
            expr = where_expr(k)
            collection.append(mydict(
                and_or = and_or,
                is_or_not = expr.is_or_not,
                column = expr.column,
                operator = expr.operator
            ))
            bindings.append(v)

        return self

    def group_by(self, *columns):
        for c in columns:
            self._group_by.append(self.__aqm(c))
        return self

    def order_by(self, column, desc=False):
        c = self.__aqm(column)
        if not desc:
            self._order_by.append(c)
        else:
            self._order_by.append('%s DESC' % c)
        return self

    def __add_quotation_marks(self, s):
        if s == '*':
            return s
        return '%s%s%s' % (self.qutotation_marks, s, self.qutotation_marks)
    __aqm = __add_quotation_marks

    @property
    def sql(self):
        sql = 'SELECT {columns} FROM {tablename}'.format(
            columns = ', '.join(map(self.__aqm, self._select)) if self._select else '*',
            tablename = self.__aqm(self.tbname)
        )
        where_sql = self.where_sql
        if where_sql:
            sql = '%s WHERE %s' % (sql, where_sql)

        if self._group_by:
            sql = '%s GROUP BY %s' % (sql, ', '.join(self._group_by))

        if self._order_by:
            sql = '%s ORDER BY %s' % (sql, ', '.join(self._order_by))

        return sql

    @property
    def where_sql(self):
        return self.__where_having_sql(self._where, self._where_bindings)

    def __where_having_sql(self, collection, bindings):
        sqls = []
        if collection:
            for i, w in enumerate(collection):
                k, op = self.__aqm(w.column), w.operator
                is_or_not, and_or = w.is_or_not, w.and_or

                v = bindings[i]
                if op == 'between':
                    # between 要做特殊处理
                    if not is_array(v) or len(v) != 2:
                        raise TypeError('Should give between 2 params, but %s' % v)
                    bt = 'NOT BETWEEN' if is_or_not == 'not' else 'BETWEEN'
                    sqls.append('%s %s %s %%s AND %%s' % (and_or, k, bt))
                    self._bindings.extend(v)
                else:
                    if is_array(v):
                        _in = 'NOT IN' if is_or_not == 'not' else 'IN'
                        sqls.append('%s %s %s (%s)' % (and_or, k, _in, ', '.join(['%s']*len(v))) )
                        self._bindings.extend(v)
                    elif v is None:
                        _is = 'IS NOT NULL' if is_or_not == 'not' else 'IS NULL'
                        sqls.append('%s %s %s' % (and_or, k, _is) )
                    else:
                        sqls.append('%s %s %s %%s' % (and_or, k, op) )
                        self._bindings.append(v)
        
        if sqls:
            s = ' '.join(sqls)
            if s.startswith('AND '):
                s = s[4:]
            elif s.startswith('OR '):
                s = s[3:]
            return s
        return ''
