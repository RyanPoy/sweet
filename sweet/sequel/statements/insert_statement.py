from typing import Optional, Self

from sweet.sequel.schema.columns import Column
from sweet.sequel.statements import Statement
from sweet.sequel.terms import literal
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
        MySQLVisitor().sql(stmt)
    Todo: support Source of ast
    """

    def __init__(self) -> None:
        super().__init__()
        self.insert_or_update = literal.INSERT
        self._column_names: [ColumnName] = []
        self._values_list: ValuesList = ValuesList()

    def into(self, table_name: TableName) -> Self:
        """
        Specifies the target table for the INSERT statement.

        :param table_name: The table name to insert into. Should be an instance of `TableName`
        :return: The current InsertStatement instance.
        """
        self._table_name = table_name
        return self

    @property
    def columns(self) -> [ColumnName]:
        """
        Gets the list of columns to insert into.

        :return: List of ColumnName instances.
        """
        return self._column_names

    def column(self, *column_names: ColumnName) -> Self:
        """
        Specifies the columns to insert into.

        :param column_names: One or more ColumnName instances to insert into.
        :return: The current InsertStatement instance.
        """
        if column_names:
            self._column_names = column_names
        return self

    @property
    def values(self) -> ValuesList:
        """
        Gets the list of values to insert.

        :return: The list of values to insert.
        """
        return self._values_list

    def insert(self, *values: DBDataType) -> Self:
        """
        Adds a new row to the INSERT statement.

        :param values: The values to insert into the specified columns.
        :return: The current InsertStatement instance.
        """
        self.__insert_or_replace(*values)
        self.insert_or_update = literal.INSERT
        return self

    def ignore(self) -> Self:
        self.insert_or_update = literal.INSERT_IGNORE
        return self

    def insert_rows(self, *rows: [DBDataType]) -> Self:
        """
        Adds multiple rows to the INSERT statement.

        :param rows: List of tuples representing rows to insert.
        :return: The current InsertStatement instance.
        """
        if rows:
            self._values_list.append(rows)
        self.insert_or_update = literal.INSERT
        return self

    def replace(self, *values: DBDataType) -> Self:
        """
        Adds a new row to the INSERT statement with a REPLACE operation.

        :param values: The values to insert into the specified columns.
        :return: The current InsertStatement instance.
        """
        self.__insert_or_replace(*values)
        self.insert_or_update = literal.REPLACE
        return self

    def replace_rows(self, *rows: [DBDataType]) -> Self:
        """
        Adds multiple rows with a REPLACE operation for batch processing.

        :param rows: List of tuples representing rows to replace.
        :return: The current InsertStatement instance.
        """
        if rows:
            self._values_list.append(rows)
        self.insert_or_update = literal.REPLACE
        return self

    def __insert_or_replace(self, *values: DBDataType) -> Self:
        """
        Internal method to handle both insert and replace operations.

        :param values: The values to insert or replace into the specified columns.
        :return: The current InsertStatement instance.
        """
        if values:
            self._values_list.append([values])
        return self

