import copy
from typing import Optional, Self, TYPE_CHECKING

from sweet.sequel.terms.values import Value1

if TYPE_CHECKING:
    from sweet.sequel.terms.binary import Binary

from sweet.sequel import Operator
from sweet.sequel.terms.literal import Literal, STAR
from sweet.utils import is_array


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

    def rm_alias(self) -> Self:
        instance = copy.copy(self)
        instance.alias = None
        return instance

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

    def eq(self, v: Value1) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS, v)
        elif is_array(v):
            return self._binary(Operator.IN, v)
        else:
            return self._binary(Operator.EQ, v)

    def not_eq(self, v: Value1) -> 'Binary':
        return self.eq(v).invert()

    def gt(self, v: Value1) -> 'Binary':
        return self._binary(Operator.GT, v)

    def not_gt(self, v: Value1) -> 'Binary':
        return self.gt(v).invert()

    def gte(self, v: Value1) -> 'Binary':
        return self._binary(Operator.GTE, v)

    def not_gte(self, v: Value1) -> 'Binary':
        return self.gte(v).invert()

    def lt(self, v: Value1) -> 'Binary':
        return self._binary(Operator.LT, v)

    def not_lt(self, v: Value1) -> 'Binary':
        return self.lt(v).invert()

    def lte(self, v: Value1) -> 'Binary':
        return self._binary(Operator.LTE, v)

    def not_lte(self, v: Value1) -> 'Binary':
        return self.lte(v).invert()

    def like(self, v: Value1) -> 'Binary':
        return self._binary(Operator.LIKE, v)

    def not_like(self, v: Value1) -> 'Binary':
        return self.like(v).invert()

    def between(self, v: [Value1]) -> 'Binary':
        if not (is_array(v) and len(v) == 2):
            raise ValueError('The between function expects a list or tuple of length 2, but it is not.')
        return self._binary(Operator.BETWEEN, v)

    def not_between(self, v: [Value1]) -> 'Binary':
        if not (is_array(v) and len(v) == 2):
            raise ValueError('The not_between function expects a list or tuple of length 2, but it is not.')
        return self._binary(Operator.NOT_BETWEEN, v)

    def regex(self, v: str) -> 'Binary':
        return self._binary(Operator.REGEX, v)

    def not_regex(self, v: str) -> 'Binary':
        return self.regex(v).invert()

    def _binary(self, op, v) -> 'Binary':
        from sweet.sequel.terms.binary import Binary
        return Binary(op, self, v)
