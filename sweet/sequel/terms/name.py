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

    def __repr__(self):
        if self.schema_name:
            return f'{self.__class__.__name__}("{self.schema_name}"."{self.value}")'
        return f'{self.__class__.__name__}("{self.value}")'


class ColumnName(Name):
    """
    Represents a column name in a SQL statement.

    Inherits from the `Name` class and is specifically used for column names in SQL queries.
    """

    def as_(self, alias: str) -> Alias:
        return Alias(self, ColumnName(alias))


class TableName(Name):
    """
    Represents a table name in a SQL statement.

    Inherits from the `Name` class and is specifically used for table names in SQL queries.
    """

    def as_(self, alias: str) -> Alias:
        return Alias(self, TableName(alias))

    def column_name_of(self, name: str) -> ColumnName:
        return ColumnName(name, self.value)


class IndexName(Name):
    pass
