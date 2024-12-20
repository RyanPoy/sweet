from typing import Optional, Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.statements import Statement
from sweet.sequel.terms.name import ColumnName, TableName
from sweet.sequel.terms.values_list import ValuesList
from sweet.utils import DBDataType


class InsertStatement(Statement):
    """
    Represents Abstract Syntax Tree(AST) for SQL INSERT statement

    The InsertStatement AST is structured as follows:

    InsertStatement
    ├── Target: TableName
    ├── Columns: ColumnNameList
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

    Usage:
        # Create a Insert statement and specify the target table
        stmt = InsertStatement().into(TableName("users"))

        # set column to insert
        stmt.column(ColumnName("id"), TableName("name"))

        # insert operation
        stmt.insert(1, "lucy")  # insert normal values
        stmt.insert(1, "Lucy").insert(2, "Lily") # chaining multiple insert for batch processing
        stmt.insert_rows([ (1, "Lucy"), (2, "Lily") ]) # listing multiple insert for batch processing
        stmt.insert(1, "Lucy").insert(2, "Lily") # chaining multiple insert or replace for batch processing
        stmt.insert_rows([ (1, "Lucy"), (2, "Lily") ]) # listing multiple insert for batch processing

        # you can also execute replace operation in the same way.
        stmt.replace(1, "lucy")  # insert normal values
        stmt.replace(1, "Lucy").replace(2, "Lily") # chaining multiple insert for batch processing
        stmt.replace_rows([ (1, "Lucy"), (2, "Lily") ]) # listing multiple insert for batch processing
        stmt.replace(1, "Lucy").replace(2, "Lily") # chaining multiple insert or replace for batch processing
        stmt.replace_rows([ (1, "Lucy"), (2, "Lily") ]) # listing multiple insert for batch processing

        # generator the sql of database (e.g. MySQL)
        stmt.sql(MySQLVisitor())
    Todo: support Source of ast
    """

    def __init__(self) -> None:
        super().__init__()
        self._column_names: [ColumnName] = []
        self._ignore = False
        self._replace = False
        self._values_list: ValuesList = ValuesList()

    def is_ignore(self):
        return self._ignore

    def is_replace(self):
        return self._replace

    def into(self, table: TableName) -> Self:
        self._table_name = table
        return self

    @property
    def columns(self) -> [ColumnName]:
        return self._column_names

    def column(self, *column_names: ColumnName) -> Self:
        if column_names:
            self._column_names = column_names
        return self

    @property
    def values(self) -> ValuesList:
        return self._values_list

    def insert(self, *values: DBDataType) -> Self:
        self.__insert_or_replace(*values)
        self._replace = False
        return self

    def insert_rows(self, *rows: [DBDataType]) -> Self:
        if rows:
            self._values_list.append(rows)
        self._replace = False
        return self

    def replace(self, *values: DBDataType) -> Self:
        self.__insert_or_replace(*values)
        self._replace = True
        return self

    def replace_rows(self, *rows: [DBDataType]) -> Self:
        if rows:
            self._values_list.append(rows)
        self._replace = True
        return self

    def __insert_or_replace(self, *values: DBDataType) -> Self:
        if values:
            self._values_list.append([values])
        return self

    def ignore(self) -> Self:
        self._ignore = True
        return self

