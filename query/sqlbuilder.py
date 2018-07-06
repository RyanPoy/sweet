#coding: utf8
from sweet.utils import *


class mydict(dict):

    def __getattr__(self, k):
        if k in self:
            return self[k]
        return super(__getattr__, k)


class where_expr(object):

    # logic, operator, 
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
            self.logic, self.column, self.operator = '', s, '='
        elif vs[-1] in self.ops: # 后缀在 ops里面
            self.logic, self.operator = self.ops[vs[-1]]
            self.column = s[:-len(vs[-1])-2]
        else:
            self.logic, self.column, self.operator = '', s, '='



class SQLBuilder(object):

    operators = {
        'gt': '>',
        'gte': '>=',
        'lt': '<',
        'lte': '<=',
    }

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
        for k, v in kwargs.items():
            expr = where_expr(k)
            self._where.append(mydict(
                logic = expr.logic,
                column = expr.column,
                operator = expr.operator
            ))
            self._where_bindings.append(v)
        return self
        # return self.__where(False, True, **kwargs)

    # def where_not(self, **kwargs):
    #     return self.__where(True, True, **kwargs)

    # def or_(self, **kwargs):
    #     return self.__where(False, False, **kwargs)

    # def or_not(self, **kwargs):
    #     return self.__where(True, False, **kwargs)

    # def __where(self, logic, **kwargs):
    #     pass
        # _in, _is, eq = 'IN', 'IS', '='
        # if _not:
        #     _in, _is, eq = 'NOT IN', 'IS NOT', '!='
        # where_list = self._where if _and else self._or

        # for k, v in kwargs.items():
        #     if is_array(v):
        #         where_sqls.append('%s %s (%s)' % (self.__aqm(k), _in, ', '.join(['%s']*len(v))))
        #         self._where_bindings.extend(v)
        #     elif v is None:
        #         where_sqls.append('%s %s NULL' % (self.__aqm(k), _is) )
        #     else:
        #         where_sqls.append('%s %s %%s' % (self.__aqm(k), eq) )
        #         self._where_bindings.append(v)

        # return self

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
                k, op, logic = w.column, w.operator, w.logic
                v = self._where_bindings[i]
                if is_array(v):
                    _in = 'NOT IN' if logic == 'not' else 'IN'
                    where_sqls.append('%s %s (%s)' % (self.__aqm(k), _in, ', '.join(['%s']*len(v))))
                    self._bindings.extend(v)
                elif v is None:
                    _is = 'IS NOT NULL' if logic == 'not' else 'IS NULL'
                    where_sqls.append('%s %s' % (self.__aqm(k), _is) )
                else:
                    where_sqls.append('%s %s %%s' % (self.__aqm(k), op) )
                    self._bindings.append(v)
            sql = '%s WHERE %s' % (sql, ' AND '.join(where_sqls))
        # if self._or:
        #     if not has_where:
        #         sql = '%s WHERE %s' % (sql, ' OR '.join(self._or))
        #     else:
        #         sql = '%s %s' % (sql, ' OR '.join(self._or))
 
        return sql
