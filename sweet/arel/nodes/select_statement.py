import copy
from typing import Self

from sweet.arel.nodes.node import Node
from sweet.arel.nodes.select_core import SelectCore
from sweet.arel.schema.table import Table


class SelectStatement(Node):

    def __init__(self, relation: Table = None):
        self.cores = [SelectCore(relation)]
        self.orders = []
        self.limit = None
        self.lock = None
        self.offset = None
        self.with_ = None

    def init_from(self, other: Self) -> Self:
        self.cores = copy.deepcopy(other.cores)
        self.orders = copy.deepcopy(other.orders)
        return self

    def __hash__(self):
        return hash(f"{self.cores}-{self.orders}-{self.limit}-{self.lock}-{self.offset}-{self.with_}")

    def __eq__(self, other):
        return self.__class__ == other.__class__ \
            and self.cores == other.cores \
            and self.orders == other.orders \
            and self.limit == other.limit \
            and self.lock == other.lock \
            and self.offset == other.offset \
            and self.with_ == other.with_

    alias = __eq__
