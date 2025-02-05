from typing import Optional, Self

from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.literal import Literal, STAR
from sweet.utils import DBDataType, is_array


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
        if schema_name is None:
            self.schema_name = None
        elif isinstance(schema_name, str):
            self.schema_name: str = schema_name
        elif isinstance(schema_name, Name):
            if schema_name.alias:
                self.schema_name = schema_name.alias
            else:
                self.schema_name = schema_name.value

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

    def eq(self, v: Self | DBDataType) -> Binary:
        if v is None:
            return Binary("IS", self, v)
        elif is_array(v):
            return Binary("IN", self, v)
        else:
            return Binary("=", self, v)

    def not_eq(self, v: Self | DBDataType) -> Binary:
        if v is None:
            return Binary("IS NOT", self, v)
        elif is_array(v):
            return Binary("NOT IN", self, v)
        else:
            return Binary("<>", self, v)

    def gt(self, v: Self | DBDataType) -> Binary:
        return Binary(">", self, v)

    def gte(self, v: Self | DBDataType) -> Binary:
        return Binary(">=", self, v)

    def lt(self, v: Self | DBDataType) -> Binary:
        return Binary("<", self, v)

    def lte(self, v: Self | DBDataType) -> Binary:
        return Binary("<=", self, v)

    def like(self, v: Self | DBDataType) -> Binary:
        return Binary("LIKE", self, v)

    def not_like(self, v: Self | DBDataType) -> Binary:
        return Binary("NOT LIKE", self, v)

    def between(self, v: [Self | DBDataType]) -> Binary:
        if not (is_array(v) and len(v) == 2):
            raise ValueError('The between function expects a list or tuple of length 2, but it is not.')
        return Binary("BETWEEN", self, v)

    def not_between(self, v: [Self | DBDataType]) -> Binary:
        if not (is_array(v) and len(v) == 2):
            raise ValueError('The not_between function expects a list or tuple of length 2, but it is not.')
        return Binary("NOT BETWEEN", self, v)
