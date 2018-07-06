#coding: utf8
from sweet.utils import *


class SQLBuilder(object):

    def __init__(self):
        self.tbname = ''
        self._select = []
        self._distinct = False
        self._where = []
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
        for k, v in kwargs.items():
            if is_array(v):
                self._where.append('%s IN (%s)' % (self.__aqm(k), ', '.join(['%s']*len(v))))
                self._where_bindings.extend(v)
            else:
                self._where.append('%s = %%s' % self.__aqm(k) )
                self._where_bindings.append(v)
        return self

    def where_not(self, **kwargs):
        for k, v in kwargs.items():
            if is_array(v):
                self._where.append('%s NOT IN (%s)' % (self.__aqm(k), ', '.join(['%s']*len(v))))
                self._where_bindings.extend(v)
            else:
                self._where.append('%s != %%s' % self.__aqm(k) )
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
        if self._where:
            sql = '%s WHERE %s' % (sql, ' AND '.join(self._where))

        return sql
