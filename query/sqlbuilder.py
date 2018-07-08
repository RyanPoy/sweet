#coding: utf8
from sweet.utils import *


class mydict(dict):

    def __getattr__(self, k):
        if k in self:
            return self[k]
        return super(__getattr__, k)


class where_expr(object):

    # is_or_not, operator, 
    ops = {
        'gt': ('', '>'), 
        'not_gt': ('not', '>'), 
        'gte': ('', '>='), 
        'not_gte': ('not', '>='), 
        'lt': ('', '<'), 
        'not_lt': ('not', '<'), 
        'lte': ('', '<='), 
        'not_lte': ('not', '<='),
        'not': ('not', '!=')
    }

    def __init__(self, s):
        vs = s.split('__')
        if len(vs) == 1: # 没有找到 "__"
            self.is_or_not, self.column, self.operator = '', s, '='
        elif vs[-1] in self.ops: # 后缀在 ops里面
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
        self._bindings = []

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
        return self.__where_or('AND', **kwargs)

    def or_(self, **kwargs):
        return self.__where_or('OR', **kwargs)

    def __where_or(self, and_or, **kwargs):
        for k, v in kwargs.items():
            expr = where_expr(k)
            self._where.append(mydict(
                and_or = and_or,
                is_or_not = expr.is_or_not,
                column = expr.column,
                operator = expr.operator
            ))
            self._where_bindings.append(v)
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
        where_sqls = []
        if self._where:
            for i, w in enumerate(self._where):
                k, op, is_or_not, and_or = w.column, w.operator, w.is_or_not, w.and_or
                v = self._where_bindings[i]
                if is_array(v):
                    _in = 'NOT IN' if is_or_not == 'not' else 'IN'
                    where_sqls.append('%s %s %s (%s)' % (and_or, self.__aqm(k), _in, ', '.join(['%s']*len(v))) )
                    self._bindings.extend(v)
                elif v is None:
                    _is = 'IS NOT NULL' if is_or_not == 'not' else 'IS NULL'
                    where_sqls.append('%s %s %s' % (and_or, self.__aqm(k), _is) )
                else:
                    where_sqls.append('%s %s %s %%s' % (and_or, self.__aqm(k), op) )
                    self._bindings.append(v)
        
        if where_sqls:
            s = ' '.join(where_sqls)
            if s.startswith('AND '):
                s = s[4:]
            elif s.startswith('OR '):
                s = s[3:]
            sql = '%s WHERE %s' % (sql, s)

        return sql
