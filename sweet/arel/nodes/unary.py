from typing import Self

from sweet.arel.nodes.node import Node


class Unary(Node):

    def __init__(self, expr):
        self.expr = expr

    # def __hash__(self):
    #     return hash(self.expr)
    #
    # def __eq__(self, other: Self):
    #     return isinstance(other, Unary) and self.expr == other.expr

    # def children(self):
    #     return [self.left]


class And(Unary): pass
class Or(Unary): pass
class Limit(Unary): pass
class Offset(Unary): pass
class Quoted(Unary): pass
class Grouping(Unary): pass