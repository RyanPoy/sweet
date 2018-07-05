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

    def __add_quotation_marks_to_column_name(self, s):
        return '"%s"' % s if s != '*' else '*'

    def to_sql(self):
        return 'SELECT {columns} FROM "{tablename}"'.format(
            columns = ', '.join(map(self.__add_quotation_marks_to_column_name, self._select)),
            tablename = self.tbname
        )
