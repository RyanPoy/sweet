from sweet.sequel.nodes.node import Node
from sweet.utils import qs


class Table:

    def __init__(self, name: str, alias: str = None, clazz: object = None, type_caster: object = None):
        self.name = name
        self.name_quoted = qs(self.name)
        self.clazz = clazz
        self.type_caster = type_caster
        if not self.type_caster and self.clazz:
            self.type_caster = self.clazz.type_caster
        if alias == name:
            alias = None
        self.alias = alias


