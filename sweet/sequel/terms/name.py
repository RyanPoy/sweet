from typing import Self

from sweet.sequel.terms import Term
from sweet.sequel.terms.alias import Alias


class Name(Term):
    """
    Represents a generic name term in a SQL statement (e.g., table name, column name, alias).

    This class serves as a base class for different types of names (such as column names,
    table names, and alias names) used in constructing SQL queries.

    Attributes:
        value (str): The name as a string (e.g., column, table, alias).
    """

    def __init__(self, name: str, schema_name: str = None) -> None:
        """
        :param name: The name of the entity (column, table, alias). (e.g., of `str`)
        """
        super().__init__()
        self.value: str = name
        self.schema_name: str = schema_name

    def as_(self, alias: str) -> Alias:
        return Alias(self, self.__class__(alias))

    def __repr__(self):
        if self.schema_name:
            return f'{self.__class__.__name__}("{self.schema_name}"."{self.value}")'
        return f'{self.__class__.__name__}("{self.value}")'

    def __hash__(self) -> int:
        return hash(f'{self.value}-{self.schema_name or ""}')

    def __eq__(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.value == other.value and self.schema_name == other.schema_name


class ColumnName(Name):
    """
    Represents a column name in a SQL statement.

    Inherits from the `Name` class and is specifically used for column names in SQL queries.
    """
    pass


class TableName(Name):
    """
    Represents a table name in a SQL statement.

    Inherits from the `Name` class and is specifically used for table names in SQL queries.
    """
    def column_name_of(self, name: str) -> ColumnName:
        return ColumnName(name, self.value)

    def __getattribute__(self, item):
        try:
            return object.__getattribute__(self, item)
        except AttributeError:
            return ColumnName(item, self.value)


class IndexName(Name):
    pass
