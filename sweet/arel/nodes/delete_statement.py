import copy

from sweet.arel.nodes.node import Node
from sweet.arel.schema.table import Table


class DeleteStatement(Node):

    def __init__(self, relation: Table = None, wheres: list = []):
        self.relation = relation
        self.wheres = wheres
        self.groups = []
        self.havings = []
        self.orders = []
        self.limit = None
        self.offset = None
        self.key = None

    @classmethod
    def copy_from(self, other):
        other = DeleteStatement()
        if self.relation:
            other.relation = copy.copy(self.relation)
        if self.wheres:
            other.wheres = copy.copy(self.wheres)

    def __hash__(self):
        return hash(f'{self.__class__}-{self.relation}-{self.wheres}-{self.orders}-{self.limit}-{self.offset}-{self.key}')

    def __eq__(self, other):
        return self.__class__ == other.__class__ \
            and self.relation == other.relation \
            and self.wheres == other.wheres \
            and self.orders == other.orders \
            and self.groups == other.groups \
            and self.havings == other.havings \
            and self.limit == other.limit \
            and self.offset == other.offset \
            and self.key == other.key
    alias = __eq__
