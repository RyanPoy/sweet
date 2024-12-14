from enum import Enum
from typing import Self

from sweet.utils import DBDataType


class OP(Enum):
    LESS_THAN = "<"
    LESS_THAN_AND_EQUAL = "<="
    GREAT_THAN = ">"
    GREAT_THAN_AND_EQUAL = ">="
    NOT = '<>',
    LIKE = 'LIKE'
    NOT_LIKE = "NOT LIKE",


class QLogic(Enum):
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'


class Q:
    def __init__(self, **kwargs: { str: DBDataType }) -> None:
        # self.operator = operator  # Logical operator: 'AND', 'OR', 'NOT'
        # self.children = children or []  # List of child nodes
        self.invert = False
        self.operator = QLogic.AND
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
            ', '.join([ str(c) for c in self.children ]),
            ")",
        ])

    def ast(self):
        return {
            'logic': self.operator,
            'condition': f"{self.field}={self.value}",
            'children': [ q.ast() for q in self.children ],
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
        return self.__combine(other, QLogic.AND)

    def __or__(self, other) -> Self:
        return self.__combine(other, QLogic.OR)

    def __invert__(self) -> Self:
        self.invert = True
        return self

    def __combine(self, other: Self, operator: QLogic) -> Self:
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
