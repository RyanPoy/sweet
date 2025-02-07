from sweet.utils import DBDataType


class Value:
    """
    Represents a value in a SQL statement.

    Usage:
        # initialize
        v = Value(43)
    """
    def __init__(self, v: DBDataType):
        self.v = v
