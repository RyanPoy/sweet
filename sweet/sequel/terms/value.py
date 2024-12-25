from sweet.sequel.terms import Term
from sweet.utils import DBDataType


class Value(Term):
    """
    Represents a value in a SQL statement.

    Usage:
        # initialize
        v = Value(43)
    """
    def __init__(self, v: DBDataType):
        self.v = v


class Regexp(Term):
    """
    Represents a value in a SQL statement.

    Usage:
        # initialize
        v = Value(43)
    """
    def __init__(self, v: DBDataType):
        self.v = v
