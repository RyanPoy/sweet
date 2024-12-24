from enum import Enum
from typing import Self

from sweet.sequel.terms import Term
from sweet.sequel.terms.pair import Pair
from sweet.utils import DBDataType


class Q(Term):
    class Logic(Enum):
        AND = 'AND'
        OR = 'OR'
        NOT = 'NOT'

        def __str__(self):
            return self.value

    def __init__(self, **kwargs: { str: DBDataType }) -> None:
        super().__init__()

        self.logic_op = Q.Logic.AND
        self.children = []
        self.condition = None

        if len(kwargs) == 1:
            self.condition = Pair(**kwargs)
        else:
            i = 0
            q = None
            for k, v in kwargs.items():
                if i == 0:
                    q = Q()
                    q.condition = Pair(**{k: v})
                else:
                    q = q & Q(**{k: v})
                i += 1
            if q is not None:
                self.children = q.children

    def __repr__(self):
        return ''.join([
            str(self.logic_op),
            "(",
            f"{self.condition}" if self.condition else "",
            ', '.join([str(c) for c in self.children]),
            ")",
        ])

    def ast(self):
        return {
            'logic'    : self.logic_op,
            'condition': f"{self.condition}",
            'children' : [q.ast() for q in self.children],
        }

    def __eq__(self, other):
        eq = self.__class__ == other.__class__ \
             and self.condition == other.condition \
             and len(self.children) == len(other.children)
        if not eq:
            return False
        for i, q in enumerate(self.children):
            if not q.__eq__(other.children[i]):
                return False
        return True

    def __hash__(self):
        return hash(f'{self.__class__}-{hash(self.condition)}-{self.logic_op}-{''.join([ hash(x) for x in self.children] )}')

    def __and__(self, other) -> Self:
        return self.__combine(other, Q.Logic.AND)

    def __or__(self, other) -> Self:
        return self.__combine(other, Q.Logic.OR)

    def __invert__(self) -> Self:
        self.invert = True
        return self

    def __combine(self, other: Self, logic_op: Logic) -> Self:
        """Combines two Q objects with a logical operator (AND, OR)."""
        if not isinstance(other, Q):
            raise TypeError("Logical operators can only be applied between two Q objects.")

        if not other.condition and not other.children:
            return self
        if not self.condition and not self.children:
            return other

        q = Q()
        q.logic_op = logic_op
        q.children = [self, other]
        return q
