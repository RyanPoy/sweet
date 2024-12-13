from sweet.arel.nodes.node import Node
from sweet.arel.schema.column import Column
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
        self._columns = {}

    def __setitem__(self, key: str, value: Column):
        if value.name in self._columns:
            raise ValueError('Column "{column.name}" already exists.')
        self._columns[value.name] = value
        value.table = self

    def __getitem__(self, item: str):
        return self._columns.get(item, None)

