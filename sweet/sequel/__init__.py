from enum import Enum
from typing import Self


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
    NOT_REGEX = "NOT REGEX"

    def invert(self) -> Self:
        if self is Operator.IS: return Operator.IS_NOT
        elif self is Operator.IS_NOT: return Operator.IS
        elif self is Operator.IN: return Operator.NOT_IN
        elif self is Operator.NOT_IN: return Operator.IN
        elif self is Operator.BETWEEN: return Operator.NOT_BETWEEN
        elif self is Operator.NOT_BETWEEN: return Operator.BETWEEN
        elif self is Operator.EQ: return Operator.NOT_EQ
        elif self is Operator.NOT_EQ: return Operator.EQ
        elif self is Operator.LT: return Operator.GTE
        elif self is Operator.LTE: return Operator.GT
        elif self is Operator.GT: return Operator.LTE
        elif self is Operator.GTE: return Operator.LT
        elif self is Operator.LIKE: return Operator.NOT_LIKE
        elif self is Operator.NOT_LIKE: return Operator.LIKE
        elif self is Operator.REGEX: return Operator.NOT_REGEX
        elif self is Operator.NOT_REGEX: return Operator.REGEX
        else: raise ValueError(f"Can't support {self.value}")

    def __str__(self) -> str:
        return self.value
