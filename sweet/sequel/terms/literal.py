from typing import Self

from sweet.sequel.terms import Term


class Literal(Term):

    def __init__(self, v):
        super().__init__()
        self.v = v

    def __str__(self) -> str:
        return str(self.v)

    def __eq__(self, other: str | Self) -> bool:
        return str(self.v) == str(other)


STAR = Literal("*")
DISTINCT = Literal("DISTINCT")

INSERT = Literal("INSERT")
INSERT_IGNORE = Literal("INSERT IGNORE")
REPLACE = Literal("REPLACE")
