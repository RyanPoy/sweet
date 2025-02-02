from typing import Optional, Self

from sweet.sequel.terms.literal import Literal, STAR


class Name:
    """
    Represents a generic name term in a SQL statement (e.g., table name, column name, alias).

    This class serves as a base class for different types of names (such as column names,
    table names, and alias names) used in constructing SQL queries.

    Attributes:
        value (str): The name as a string (e.g., column, table, alias).
    """

    def __init__(self, name: str, schema_name: str | Self = None) -> None:
        """
        :param name: The name of the entity (column, table, alias). (e.g., of `str`)
        """
        super().__init__()
        self.value: str = name
        self.alias: Optional[str] = None
        self.schema_name = None
        if schema_name is not None:
            self.schema_name: str = schema_name if isinstance(schema_name, str) else schema_name.value

    def as_(self, alias: str) -> Self:
        self.alias = alias
        return self

    def __repr__(self):
        if self.schema_name:
            return f'{self.__class__.__name__}("{self.schema_name}"."{self.value}")'
        return f'{self.__class__.__name__}("{self.value}")'

    def __hash__(self) -> int:
        return hash(f'{self.value}-{self.schema_name or ""}')

    def __eq__(self, other: str | Literal | Self) -> bool:
        if isinstance(other, str) and other == '*' and self.value == '*':
            return True
        if isinstance(other, Literal) and other == STAR and self.value == '*':
            return True
        return self.__class__ == other.__class__ and self.value == other.value and self.schema_name == other.schema_name and self.alias == other.alias
