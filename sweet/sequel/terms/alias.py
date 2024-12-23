from sweet.sequel.terms import Term


class Alias(Term):
    """
    Represents an alias name in a SQL statement.

    Inherits from the `Name` class and is specifically used for alias names in SQL queries.
    """
    def __init__(self, origin: "Name", target: "Name"):
        super().__init__()
        self.origin = origin
        self.target = target
