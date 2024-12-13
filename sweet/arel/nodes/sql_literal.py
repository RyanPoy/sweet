from sweet.arel.nodes.node import Node


class SqlLiteral(str):

    def __init__(self, value: str):
        super()
        self.value = value

    def to_sql(self):
        return self.value

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        return isinstance(other, SqlLiteral) and self.value == other.value
