from sweet.sequel.terms import Term


class Name(Term):
    """
    Represents a generic name term in a SQL statement (e.g., table name, column name, alias).

    This class serves as a base class for different types of names (such as column names,
    table names, and alias names) used in constructing SQL queries.

    Attributes:
        value (str): The name as a string (e.g., column, table, alias).
    """

    def __init__(self, name: str):
        """
        :param name: The name of the entity (column, table, alias). (e.g., of `str`)
        """
        super().__init__()
        self.value: str = name

    def __repr__(self):
        return f'{self.__class__.__name__}("{self.value}")'


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
    pass


class AliasName(Name):
    """
    Represents an alias name in a SQL statement.

    Inherits from the `Name` class and is specifically used for alias names in SQL queries.
    """
    pass
