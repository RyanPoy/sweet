from typing import Self

from sweet.sequel import Operator
from sweet.sequel.quoting import quote
from sweet.sequel.terms.binary import parse
from sweet.sequel.terms.name import Name
from sweet.utils import is_array


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

    def __init__(self, **kwargs) -> None:
        if len(kwargs) > 1:
            raise ValueError("Only one parameter is allowed for construction.")

        symbol, value = next(iter(kwargs.items()))
        binary = parse(symbol, value)
        self.field = binary.key
        self.operator = binary.op
        self.value = binary.value

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


