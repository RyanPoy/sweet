from enum import Enum
from typing import Self
from sweet.utils import DBDataType


class Q:
    class Logic(Enum):
        AND = 'AND'
        OR = 'OR'
        NOT = 'NOT'

    def __init__(self, **kwargs: { str: DBDataType }) -> None:
        # self.operator = operator  # Logical operator: 'AND', 'OR', 'NOT'
        # self.children = children or []  # List of child nodes
        self.invert = False
        self.operator = Q.Logic.AND
        self.children = []
        self.field = None
        self.value = None
        for k, v in kwargs.items():
            self.field = k
            self.value = v

    def __repr__(self):
        return ''.join([
            str(self.operator),
            "(",
            f"{self.field}={self.value}" if self.field else ""
                                                            ', '.join([str(c) for c in self.children]),
            ")",
        ])

    def ast(self):
        return {
            'logic'    : self.operator,
            'condition': f"{self.field}={self.value}",
            'children' : [q.ast() for q in self.children],
        }

    def __eq__(self, other):
        eq = self.__class__ == other.__class__ \
             and self.field == other.field \
             and self.value == other.value \
             and len(self.children) == len(other.children)
        if not eq:
            return False
        for i, q in enumerate(self.children):
            if not q.__eq__(other.children[i]):
                return False
        return True

    def __and__(self, other) -> Self:
        return self.__combine(other, Q.Logic.AND)

    def __or__(self, other) -> Self:
        return self.__combine(other, Q.Logic.OR)

    def __invert__(self) -> Self:
        self.invert = True
        return self

    def __combine(self, other: Self, operator: Logic) -> Self:
        """Combines two Q objects with a logical operator (AND, OR)."""
        if not isinstance(other, Q):
            raise TypeError("Logical operators can only be applied between two Q objects.")

        if not other.field and not other.children:
            return self
        if not self.field and not self.children:
            return other
        q = Q()
        q.operator = operator
        q.children = [self, other]
        return q
