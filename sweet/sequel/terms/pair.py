from typing import Self

from sweet.sequel import Operator
from sweet.sequel.quoting import quote
from sweet.sequel.terms.name import Name
from sweet.utils import DBDataType, is_array


class Pair:
    """
    Represents a SQL condition, which can include comparisons, logical checks, and containment checks.

    A Condition instance is built from a single key-value pair. Special conditions
    are indicated using separators (default: '__') in the key.

    Usage:
        # Normal
        pair = Pair(column1=10)

        # Special condition with operator
        pair = Pair(column1__gte=10)

        # Complex conditions with lists or tuples
        pair = Pair(column1__bt=(5, 15))
    """

    SEPERATOR = '__'

    def __init__(self, **kwargs) -> None:
        if len(kwargs) > 1:
            raise ValueError("Only one parameter is allowed for construction.")

        k, v = next(iter(kwargs.items()))
        op = None

        if self.SEPERATOR not in k:
            if v is None:
                op = Operator.IS
            elif is_array(v):
                op = Operator.IN
            else:
                op = Operator.EQ
        else:
            original_key = k
            vs = k.split(self.SEPERATOR)
            k, op = self.SEPERATOR.join(vs[:-1]), vs[-1]
            if op == 'not':
                if v is None:
                    op = Operator.IS_NOT
                elif isinstance(v, (tuple, list)):
                    op = Operator.NOT_IN
                else:
                    op = Operator.NOT_EQ
            elif op == 'bt' or op == 'not_bt':
                if not (is_array(v) and len(v) == 2):
                    raise ValueError(f'The {op} operation expects a list or tuple of length 2, but it is not.')
                op = Operator.BETWEEN if op == 'bt' else Operator.NOT_BETWEEN
            elif op == 'lt':
                op = Operator.LT
            elif op == 'lte':
                op = Operator.LTE
            elif op == 'gt':
                op = Operator.GT
            elif op == 'gte':
                op = Operator.GTE
            elif op == 'like':
                op = Operator.LIKE
            elif op == 'not_like':
                op = Operator.NOT_LIKE
            elif op == 'regex':
                op = Operator.REGEX
            else:
                if v is None:
                    op = Operator.IS
                elif is_array(v):
                    op = Operator.IN
                else:
                    op = Operator.EQ
                k = original_key
        self.field = k
        self.value = v
        self.operator = op

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
