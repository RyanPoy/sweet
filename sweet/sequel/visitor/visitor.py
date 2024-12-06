from abc import ABC, abstractmethod

from sweet.sequel.nodes.node import Node
from sweet.sequel.schema.table import Table
from sweet.utils.collectors import SQLString


class Visitor(ABC):

    def dispatch(self, name):
        name = name.split('.')[-1]
        func_name = f'visit_{name}'
        func = self.__getattribute__(func_name)
        return func
    #
    # def accept(self, node: Node | Table, collector: SQLString = None):
    #     return self.visit(node, collector)

    def visit(self, node: Node | Table, collector: SQLString = None) -> SQLString:
        method = self.dispatch(node.__class__.__name__)
        if collector is None:
            collector = SQLString()
        collector = method(node, collector)
        return collector

    def holder_parens_s(self, length: int, begin: int) -> (str, int):
        """ holder_parens_s(5, 3) -> '(?, ?, ?, ?, ?)'
        """
        s, n = self.holder_lst(self.placeholder, length, begin)
        return f'({s})', n

    @abstractmethod
    def holder_lst(self, placeholder: str, length: int, begin: int) -> (str, int):
        pass
