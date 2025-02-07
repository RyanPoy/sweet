from typing import Self

from sweet.sequel.terms import Logic
from sweet.sequel.terms.binary import Binary, parse
from sweet.utils import DBDataType


class Q:

    def __init__(self, **kwargs: {str: DBDataType}) -> None:
        self.logic_op = Logic.AND
        self.children = []
        self.binary = None
        self.invert = False

        if len(kwargs) == 1:
            self.binary = parse(**kwargs)
        else:
            i = 0
            q = None
            for k, v in kwargs.items():
                if i == 0:
                    q = Q()
                    q.binary = parse(**{k: v})
                else:
                    q = q & Q(**{k: v})
                i += 1
            if q is not None:
                self.children = q.children

    def is_empty(self) -> bool:
        return not self.children and not self.binary

    def ast(self):
        return {
            'logic'    : self.logic_op,
            'condition': f"{self.binary}",
            'children' : [q.ast() for q in self.children],
        }

    def __repr__(self):
        return ''.join([
            str(self.logic_op),
            "(",
            f"{self.binary}" if self.binary else "",
            ', '.join([str(c) for c in self.children]),
            ")",
        ])

    def __eq__(self, other):
        equals = self.__class__ == other.__class__ \
             and self.binary == other.binary \
             and len(self.children) == len(other.children)
        if not equals:
            return False
        for i, q in enumerate(self.children):
            if not q.__eq__(other.children[i]):
                return False
        return True

    def __hash__(self):
        return hash(f'{self.__class__}-{str(self.binary)}-{self.logic_op}-{''.join([str(x) for x in self.children])}')

    def __and__(self, other) -> Self:
        return self.__combine(other, Logic.AND)

    def __or__(self, other) -> Self:
        return self.__combine(other, Logic.OR)

    def __invert__(self) -> Self:
        self.invert = not self.invert
        return self

    def __combine(self, other: Self, logic_op: Logic) -> Self:
        """Combines two Q objects with a logical operator (AND, OR)."""
        if not isinstance(other, Q):
            raise TypeError("Logical operators can only be applied between two Q objects.")

        if not other.binary and not other.children:
            return self
        if not self.binary and not self.children:
            return other

        q = Q()
        q.logic_op = logic_op
        q.children = [self, other]
        return q
