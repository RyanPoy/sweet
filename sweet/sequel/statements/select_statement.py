from typing import Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement


class SelectStatement(Statement):

    def __init__(self):
        self.table = None
        self.columns = []

    def from_(self, table: Table) -> Self:
        self.table = table
        return self

    def select(self, *columns: Column) -> Self:
        if columns:
            self.columns.extend(columns)
            if not self.table:
                self.table = columns[0].table
        return self

