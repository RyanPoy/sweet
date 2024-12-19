from typing import Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement
from sweet.sequel.terms.alias import Alias


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

    def from_(self, table_or_select: Table | Self) -> Self:
        found = False
        if isinstance(table_or_select, Table):
            for t in self.tables:
                if t.name == table_or_select.name:
                    found = True
            if not found:
                self.tables.append(table_or_select)
        else:
            self.tables.append(table_or_select)
        return self

    def select(self, *columns: Column | Alias) -> Self:
        if columns:
            self.columns.extend(columns)
            if not self.tables:
                for c in columns:
                    tb = c.table if isinstance(c, Column) else c.target.table
                    self.from_(tb)
                    break
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
