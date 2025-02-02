from enum import Enum
from typing import Self

from sweet.sequel.quoting import quote
from sweet.sequel.terms.name import Name
from sweet.utils import DBDataType


class Operator(Enum):
    IS = "IS"
    IS_NOT = "IS NOT"
    IN = "IN"
    NOT_IN = "NOT IN"
    BETWEEN = "BETWEEN"
    NOT_BETWEEN = "NOT BETWEEN"
    EQ = "="
    NOT_EQ = "<>"
    LT = "<"
    LTE = "<="
    GT = ">"
    GTE = ">="
    LIKE = "LIKE"
    NOT_LIKE = "NOT LIKE"
    REGEX = "REGEX"

    def __str__(self) -> str:
        return self.value


class Pair:
    """
    Represents a SQL condition, which can include comparisons, logical checks, and containment checks.

    A Condition instance is built from a single key-value pair. Special conditions
    are indicated using separators (default: '__') in the key.

    Usage:
        # Normal
        condition = Condition(column1=10)

        # Special condition with operator
        condition = Condition(column1__gte=10)

        # Complex conditions with lists or tuples
        condition = Condition(column1__bt=(5, 15))
    """

    SEPERATOR = '__'

    def __init__(self, **kwargs) -> None:
        if len(kwargs) > 1:
            raise ValueError("Only one parameter is allowed for construction.")

        super().__init__()

        self.field = None
        self.value = None
        self.operator = Operator.EQ
        for k, v in kwargs.items():
            if self.SEPERATOR not in k:
                self._parse_normal(k, v)
            else:
                self._parse_special(k, v)
            break

    def _parse_normal(self, key: str, value: DBDataType | Name) -> None:
        """
        Parses a normal condition without a special operator.

        :param key: The field name.
        :param value: The value to compare or check.
        """
        self.field = key
        self.value = value
        if value is None:
            self.operator = Operator.IS
        elif isinstance(value, (tuple, list)):
            self.operator = Operator.IN
        else:
            self.operator = Operator.EQ

    def _parse_special(self, key: str, value: DBDataType | Name) -> None:
        """
        Parses a condition with a special operator.

        :param key: The field name and operator, separated by the SEPERATOR.
        :param value: The value to compare or check.
        :raises ValueError: If the provided operator or value format is invalid.
        """
        vs = key.split(self.SEPERATOR)
        col = self.SEPERATOR.join(vs[:-1])
        op_type = vs[-1]
        # col, op_type = vs[0], vs[1]
        self.field = col
        self.value = value
        if op_type == 'not':
            if value is None:
                self.operator = Operator.IS_NOT
            elif isinstance(value, (tuple, list)):
                self.operator = Operator.NOT_IN
            else:
                self.operator = Operator.NOT_EQ
        elif op_type == 'bt' or op_type == 'not_bt':
            if not isinstance(value, (tuple, list)) or not len(value) == 2:
                raise ValueError(f'The {op_type} operation expects a list or tuple of length 2, but it is not.')
            self.operator = Operator.BETWEEN if op_type == 'bt' else Operator.NOT_BETWEEN
        elif op_type == 'lt':
            self.operator = Operator.LT
        elif op_type == 'lte':
            self.operator = Operator.LTE
        elif op_type == 'gt':
            self.operator = Operator.GT
        elif op_type == 'gte':
            self.operator = Operator.GTE
        elif op_type == 'like':
            self.operator = Operator.LIKE
        elif op_type == 'not_like':
            self.operator = Operator.NOT_LIKE
        elif op_type == 'regex':
            self.operator = Operator.REGEX
        else:
            self._parse_normal(key, value)

    def __repr__(self) -> str:
        return f'{self.field} {self.operator} {self.value}'

    def __eq__(self, other: Self) -> bool:
        return self.__basic_eq(other) and other.operator == self.operator

    def __hash__(self):
        return hash(f"{self.__class__}-{self.field}-{quote(self.value)}-{self.operator}")

    def __basic_eq(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.field == other.field \
            and (
                self.value == other.value or (
                    isinstance(self.value, (tuple, list)) and isinstance(other.value, (tuple, list)) and list(self.value) == list(other.value)
                )
            )
