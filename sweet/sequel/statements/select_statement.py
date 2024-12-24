from typing import Self

from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement
from sweet.sequel.terms import literal
from sweet.sequel.terms.alias import Alias
from sweet.sequel.terms.name import IndexName, Name, TableName
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class SelectStatement(Statement):
    """
    SELECT
      ├── Columns
      ├── From
      │   └── table
      ├── Where
      │   └── Condition
      │       ├── column
      │       └── operator
      │       └── value
      ├── Group By
      │   ├── columns
      ├── Where
      │   └── Condition
      │       ├── column
      │       ├── operator
      │       └── value
      └── Order By
          ├── columns
          └── DESC / ASC
    """
    def __init__(self):
        super().__init__()

        self.tables = []
        self.columns = []
        self._distinct = False
        self._limit = 0
        self._offset = 0
        self.wheres = []
        self.force_indexes = []
        self.use_indexes = []

    def from_(self, table: TableName | Alias | Self) -> Self:
        found = False
        if isinstance(table, Table):
            for t in self.tables:
                if t.name == table.name:
                    found = True
            if not found:
                self.tables.append(table)
        else:
            self.tables.append(table)
        return self

    def select(self, *columns: Name | Alias | DBDataType) -> Self:
        for c in columns:
            if isinstance(c, (Name, Alias)):
                self.columns.append(c)
            elif c == '*':
                self.columns.append(literal.STAR)
            else:
                self.columns.append(Value(c))

        return self

    def force_index(self, *indexes: IndexName) -> Self:
        self.force_indexes.extend(indexes)
        return self

    def use_index(self, *indexes: IndexName) -> Self:
        self.use_indexes.extend(indexes)
        return self

    def distinct(self) -> Self:
        self._distinct = True
        return self

    def is_distinct_required(self) -> bool:
        return self._distinct

    def limit(self, limit) -> Self:
        self._limit = limit
        return self

    def offset(self, offset) -> Self:
        self._offset = offset
        return self

    def where(self, *qs: Q, **kwargs) -> Self:
        """
        Add filtering conditions for the UPDATE statement.

        :param qs: `Q` objects represents filter conditions.
        :param kwargs: keyword arguments for creating filter conditions. (e.g., `id=1`)
        :return: The current UpdateStatement instance
        """
        for q in qs:
            self.wheres.append(q)
        if kwargs:
            self.wheres.append(Q(**kwargs))
        return self

    # def __getattribute__(self, item):
    #     try:
    #         return object.__getattribute__(self, item)
    #     except AttributeError:
    #         for c in self.columns:
    #             if c.name == item:
    #                 return c
    #     return super().__getattribute__(item)
