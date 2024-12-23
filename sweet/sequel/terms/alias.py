from sweet.sequel.terms import Term


class Alias(Term):
    """
    Represents an alias name in a SQL statement.

    It is used to create alias for table or column in SQL AST,
    improving readability and enabling flexible referencing.

    Usage:
        # initialize an alias for a table
        Alias(TableName("users"), TableName("employee"))

        # initialize an alias for a table
        Alias(ColumnName("id"), ColumnName("user_id"))

        # create a table alias from a TableName
        alias = TableName("users").as_("employee")

        # create a table alias from a ColumnName
        alias = ColumnName("id").as_("user_id")
    """
    def __init__(self, origin: "Name", target: "Name"):
        super().__init__()
        self.origin = origin
        self.target = target
