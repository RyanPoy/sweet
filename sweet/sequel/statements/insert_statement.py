from typing import Self
from sweet.sequel.terms import literal
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.terms.returings import Returnings
from sweet.sequel.terms.values import Values
from sweet.sequel.types import ArrayType


class InsertStatement:
    """
    Represents Abstract Syntax Tree(AST) for SQL INSERT statement

    The InsertStatement AST is structured as follows:

    InsertStatement
    ├── Target: Name
    ├── Columns: NameList
    │   ├── Name: "column1"
    │   ├── Name: "column2"
    │   └── Name: "column3"
    ├── Values: ValueList (or Source if SELECT)
    │   ├── Value: Literal(value1)
    │   ├── Value: Literal(value2)
    │   └── Value: Literal(value3)
    └── Source: SelectStatement (optional)
        ├── Target: Name (e.g., another_table)
        ├── Columns: ColumnList
        │   ├── Name: "column1"
        │   ├── Name: "column2"
        │   └── Name: "column3"
        └── Filters: (e.g., WHERE conditions)
            └── Condition: column1 > Literal(10)

    Usage:
        # Create a Insert statement and specify the target table
        stmt = InsertStatement(Name("users"))

        # set column to insert
        stmt.column(Name("id"), Name("name"))

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

    def __init__(self, table_name: Name) -> None:
        self.insert_or_update = literal.INSERT
        self.table_name: Name = table_name
        self._column_names: [Name] = []
        self.values: Values = Values()
        self._retunings = Returnings()

    @property
    def columns(self) -> [Name]:
        return self._column_names

    def column(self, *column_names: Name) -> Self:
        if column_names:
            self._column_names = column_names
        return self

    def insert(self, *args: ArrayType) -> Self:
        if args:
            self.values.append(*args)
        self.insert_or_update = literal.INSERT
        return self

    def ignore(self) -> Self:
        self.insert_or_update = literal.INSERT_IGNORE
        return self

    def replace(self, *args: ArrayType) -> Self:
        if args:
            self.values.append(*args)
        self.insert_or_update = literal.REPLACE
        return self

    @property
    def returnings(self) -> Returnings:
        return self._retunings

    def returning(self, *cols: [Name]) -> Self:
        self._retunings.append(*cols)
        return self
