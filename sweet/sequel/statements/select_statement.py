from typing import Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement
from sweet.sequel.terms.alias import Alias


class SelectStatement(Statement):

    def __init__(self):
        self.table = None
        self.columns = []
        self._distinct = False

    def from_(self, table: Table) -> Self:
        self.table = table
        return self

    def select(self, *columns: Column | Alias) -> Self:
        if columns:
            self.columns.extend(columns)
            if not self.table:
                for c in columns:
                    self.table = c.table if isinstance(c, Column) else c.target.table
                    break
        return self

    def distinct(self) -> Self:
        self._distinct = True
        return self
