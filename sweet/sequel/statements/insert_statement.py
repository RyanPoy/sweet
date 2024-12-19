from typing import Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.statements import Statement
from sweet.sequel.terms.values_list import Values
from sweet.utils import DBDataType


class InsertStatement(Statement):
    """
    InsertStatement
    ├── Target: TableName
    │   └── Name: "table_name"
    ├── Columns: ColumnList
    │   ├── ColumnName: "column1"
    │   ├── ColumnName: "column2"
    │   └── ColumnName: "column3"
    ├── Values: ValueList (or Source if SELECT)
    │   ├── Value: Literal(value1)
    │   ├── Value: Literal(value2)
    │   └── Value: Literal(value3)
    └── Source: SelectStatement (optional)
        ├── Target: TableName (e.g., another_table)
        ├── Columns: ColumnList
        │   ├── ColumnName: "column1"
        │   ├── ColumnName: "column2"
        │   └── ColumnName: "column3"
        └── Filters: (e.g., WHERE conditions)
            └── Condition: column1 > Literal(10)

    Todo: support Source of ast
    """

    def __init__(self) -> None:
        self.table = None
        self._columns = None
        self._ignore = False
        self._replace = False
        self.values = Values()

    def is_ignore(self):
        return self._ignore

    def is_replace(self):
        return self._replace

    def into(self, table: "Table") -> Self:
        self.table = table
        return self

    def columns(self, *columns: Column) -> Self:
        if columns:
            self._columns = columns
        return self

    def replace(self, *values: DBDataType) -> Self:
        self.__insert_or_replace(*values)
        self._replace = True
        return self

    def insert(self, *values: DBDataType) -> Self:
        self.__insert_or_replace(*values)
        self._replace = False
        return self

    def __insert_or_replace(self, *values: DBDataType) -> Self:
        if values:
            self.values.append([values])
        return self

    def ignore(self) -> Self:
        self._ignore = True
        return self

    def insert_rows(self, *rows: [DBDataType]) -> Self:
        if rows:
            self.values.append(rows)
        return self
