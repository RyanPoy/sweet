from abc import ABC, abstractmethod

from sweet.sequel import SqlLiteral, quoting
from sweet.sequel.nodes.insert_statement import InsertStatement
from sweet.sequel.nodes.node import Node
from sweet.sequel.nodes.values_list import ValuesList
from sweet.sequel.schema.table import Table
from sweet.sequel.visitor.visitor import Visitor
from sweet.utils.collectors import SQLString


class ToSql(Visitor, ABC):

    @abstractmethod
    def quote_column_name(self, name: str) -> str:
        raise NotImplementedError

    @abstractmethod
    def quote_table_name(self, name: str) -> str:
        raise NotImplementedError

    def maybe_visit(self, thing: Node | Table, collector: SQLString) -> SQLString:
        if not thing:
            return collector
        collector << " "
        return self.visit(thing, collector)

    def visit_SqlLiteral(self, o: SqlLiteral, collector: SQLString) -> SQLString:
        collector << str(o)
        return collector

    def visit_ValuesList(self, o: ValuesList, collector: SQLString) -> SQLString:
        collector << "VALUES "
        for i, row in enumerate(o.rows):
            if i != 0:
                collector << ", "
            collector << "("
            for k, value in enumerate(row):
                if k != 0:
                    collector << ", "
                # if isinstance(value, [SqlLiteral, Nodes::BindParam, ActiveModel::Attribute]):
                if isinstance(value, SqlLiteral):
                    collector = self.visit(value, collector)
                else:
                    collector << quoting.quote(value)
            collector << ")"
        return collector

    def visit_InsertStatement(self, o: InsertStatement, collector: SQLString) -> SQLString:
        collector << "INSERT INTO "
        self.visit(o.relation, collector)

        if o.columns:
            collector << " ("
            for idx, c in enumerate(o.columns):
                if idx != 0:
                    collector << ", "
                collector << self.quote_column_name(f'"{c.name}"')
            collector << ")"

        if o.values:
            self.maybe_visit(o.values, collector)
        elif o.select:
            self.maybe_visit(o.select, collector)
        return collector

    def visit_Table(self, o: Table, collector):
        # if issubclass(o.__class__, Node):  # other Node
        #     self.visit(o, collector)
        if issubclass(o.__class__, Table):  # Table
            collector << self.quote_table_name(f'"{o.name}"')
        if o.alias:
            collector << " " << self.quote_table_name(f'"{o.table_alias}"')
        return collector
