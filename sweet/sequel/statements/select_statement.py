from typing import Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement
from sweet.sequel.terms.alias import Alias
from sweet.sequel.terms.name import Name, TableName


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
        self.tables = []
        self.columns = []
        self._distinct = False

    def from_(self, table: TableName | Alias) -> Self:
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

    def select(self, *columns: Name | Alias) -> Self:
        if columns:
            self.columns.extend(columns)
        return self

    def distinct(self) -> Self:
        self._distinct = True
        return self

    def is_distinct_required(self) -> bool:
        return self._distinct
    #
    # def __getattribute__(self, item):
    #     try:
    #         return object.__getattribute__(self, item)
    #     except AttributeError:
    #         for c in self.columns:
    #             if c.name == item:
    #                 return c
    #     return super().__getattribute__(item)
