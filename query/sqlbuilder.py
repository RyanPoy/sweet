#coding: utf8


class SQLBuilder(object):

    def __init__(self):
        self.tbname = ''
        self._select = []
        self._distinct = False

    def distinct(self):
        self._distinct = True

    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self._select.append(c)
        return self

    def from_(self, tbname):
        self.tbname = tbname
        return self

    def __add_quotation_marks(self, s):
        if s == '*':
            return s
        return '%s%s%s' % (self.qutotation_marks, s, self.qutotation_marks)

    def to_sql(self):
        return 'SELECT {columns} FROM {tablename}'.format(
            columns = ', '.join(map(self.__add_quotation_marks, self._select)),
            tablename = self.__add_quotation_marks(self.tbname)
        )
