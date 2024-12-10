from typing import Self

from sweet.sequel import SqlLiteral
from sweet.sequel.managers.tree_manager import TreeManager
from sweet.sequel.nodes.insert_statement import InsertStatement
from sweet.sequel.nodes.values_list import ValuesList
from sweet.sequel.schema.column import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.visitor.visitor import Visitor


class InsertManager(TreeManager):

    def __init__(self):
        super().__init__()

        self.table = None
        self.select = None
        self.ast = InsertStatement()

    def into(self, table: Table) -> Self:
        self.ast.relation = table
        return self

    @property
    def columns(self):
        return self.ast.columns

    @property
    def values(self) -> ValuesList:
        return self.ast.values

    @values.setter
    def values(self, val: ValuesList):
        self.ast.values = val

    def create_values(self, values):
        return ValuesList([values])

    def create_values_list(self, rows: []):
        return ValuesList(rows)

    def select(self, select):
        self.ast.select = select

    def insert(self, fields: [Column, any]):
        if not fields: return

        if isinstance(fields, str):
            self.ast.values = SqlLiteral(fields)
        elif not self.ast.relation:
            self.ast.relation = fields[0][0].relation

        values = []

        for column, value in fields:
            self.ast.columns.append(column)
            values.append(value)
        self.ast.values = self.create_values(values)
        return self

# def insert(self, records: list[dict] = None, **kwargs) -> Self:
#     insert_list = [dict]
#     if records:
#         if is_hash(records):
#             insert_list.append(records)
#         elif is_array(records):
#             insert_list.extend(records)
#     if kwargs:
#         insert_list.append(kwargs)
#     for record in insert_list:
#         for k, v in record:
#
#
#     return self
