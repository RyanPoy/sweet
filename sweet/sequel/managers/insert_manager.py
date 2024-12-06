from typing import Self

from sweet.sequel.nodes.insert_statement import InsertStatement
from sweet.sequel.nodes.values_list import ValuesList
from sweet.sequel.table import Table
from sweet.sequel.visitor.visitor import Visitor
from sweet.utils import is_hash, is_array


class InsertManager:

    def __init__(self):
        self.table = None
        self.select = None
        self.ast = InsertStatement()

    def into(self, table: Table) -> Self:
        self.ast.relation = table
        return self

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

    def to_sql(self, visitor: Visitor):
        return visitor.visit(self.ast)

#   def insert(fields)
#     return if fields.empty?
#
#     if String === fields
#       @ast.values = Nodes::SqlLiteral.new(fields)
#     else
#       @ast.relation ||= fields.first.first.relation
#
#       values = []
#
#       fields.each do |column, value|
#         @ast.columns << column
#         values << value
#       end
#       @ast.values = create_values(values)
#     end
#     self
#   end


#
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
