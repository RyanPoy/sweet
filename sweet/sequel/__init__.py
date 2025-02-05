from enum import Enum


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

    def __str__(self) -> str:
        return self.value
