class Column:

    def __init__(self, name):
        self.name = name
        self._table = None

    @property
    def table(self):
        return self._table

    @table.setter
    def table(self, t: table):
        self._table = t

    @property
    def relation(self):
        return self._table
