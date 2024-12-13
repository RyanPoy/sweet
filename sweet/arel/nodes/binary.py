import copy
from typing import Self


from sweet.arel.nodes.node import Node


class Binary(Node):

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def init_copy(self, other: Self) -> Self:
        if other.left:
            self.left = copy.deepcopy(other.left)
        if other.right:
            self.right = copy.deepcopy(other.right)
        return self

    def __hash__(self):
        return hash(f'f{self.__class__}-{self.left}-{self.right}')

    def __eq__(self, other: Self):
        return self.__class__ == other.__class__ and self.left == other.left and self.right == other.right

    alias = __eq__


class As(Binary):
    def to_cte(self):
        from sweet.arel.nodes.cte import Cte
        return Cte(self.left.name, self.right)

class Between(Binary):
    pass

class GreaterThan(Binary):
    def invert(self):
        return LessThanOrEqual(self.left, self.right)


class GreaterThanOrEqual(Binary):
    def invert(self):
        LessThan(self.left, self.right)


class LessThan(Binary):
    def invert(self) -> GreaterThanOrEqual:
        return GreaterThanOrEqual(self.left, self.right)


class LessThanOrEqual(Binary):
    def invert(self) -> GreaterThan:
        return GreaterThan(self.left, self.right)


class IsDistinctFrom(Binary):
    def invert(self):
        return IsNotDistinctFrom(self.left, self.right)


class IsNotDistinctFrom(Binary):
    def invert(self) -> IsDistinctFrom:
        return IsDistinctFrom(self.left, self.right)


class NotEqual(Binary):
    def invert(self):
        return Equality(self.left, self.right)


class NotIn(Binary):
    def invert(self):
        return In(self.left, self.right)


class In(Binary):

    def is_equality(self) -> bool:
        return True

    def invert(self) -> NotIn:
        return NotIn(self.left, self.right)


class Equality(Binary):

    def is_equality(self) -> bool:
        return True

    def invert(self):
        NotEqual(self.left, self.right)
