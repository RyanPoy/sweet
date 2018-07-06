#coding: utf8
from sweet.utils import *


class SQLBuilder(object):

    def __init__(self):
        self.tbname = ''
        self._select = []
        self._distinct = False
        self._where = []
        self._or = []
        self._where_bindings = []

    def distinct(self):
        self._distinct = True

    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self._select.append(c)
        return self

    @property
    def bindings(self):
        return self._where_bindings

    def from_(self, tbname):
        self.tbname = tbname
        return self

    def where(self, **kwargs):
        return self.__where_or_where_not(False, True, **kwargs)

    def where_not(self, **kwargs):
        return self.__where_or_where_not(True, True, **kwargs)

    def or_(self, **kwargs):
        return self.__where_or_where_not(False, False, **kwargs)

    def or_not(self, **kwargs):
        return self.__where_or_where_not(True, False, **kwargs)

    def __where_or_where_not(self, _not, _and, **kwargs):
        _in, _is, eq = 'IN', 'IS', '='
        if _not:
            _in, _is, eq = 'NOT IN', 'IS NOT', '!='
        where_list = self._where if _and else self._or

        for k, v in kwargs.items():
            if is_array(v):
                where_list.append('%s %s (%s)' % (self.__aqm(k), _in, ', '.join(['%s']*len(v))))
                self._where_bindings.extend(v)
            elif v is None:
                where_list.append('%s %s NULL' % (self.__aqm(k), _is) )
            else:
                where_list.append('%s %s %%s' % (self.__aqm(k), eq) )
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
        has_where = False
        if self._where:
            has_where = True
            sql = '%s WHERE %s' % (sql, ' AND '.join(self._where))
        if self._or:
            if not has_where:
                sql = '%s WHERE %s' % (sql, ' OR '.join(self._or))
            else:
                sql = '%s %s' % (sql, ' OR '.join(self._or))
 
        return sql
