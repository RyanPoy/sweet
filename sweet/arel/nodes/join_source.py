from sweet.arel.nodes.node import Node
from sweet.arel.schema.table import Table


class JoinSource(Node):

    def __init__(self, relation: Table = None):
        self.relation = relation
