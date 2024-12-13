import copy
from abc import ABC, abstractmethod

from sweet.arel import SqlLiteral, quoting
from sweet.arel.nodes.binary import In
from sweet.arel.nodes.delete_statement import DeleteStatement
from sweet.arel.nodes.insert_statement import InsertStatement
from sweet.arel.nodes.join_source import JoinSource
from sweet.arel.nodes.node import Node
from sweet.arel.nodes.select_statement import SelectStatement
from sweet.arel.nodes.unary import Offset, Limit, Grouping, Quoted
from sweet.arel.nodes.values_list import ValuesList
from sweet.arel.schema.column import Column
from sweet.arel.schema.table import Table
from sweet.arel.visitor.visitor import Visitor
from sweet.utils import is_array
from sweet.utils.collectors import SQLString


class TreeStatement:
    pass


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

    def visit_Quoted(self, o: Quoted, collector: SQLString) -> SQLString:
        if issubclass(o.expr.__class__, Node):  # other Node
            self.visit(o.expr, collector)
        elif isinstance(o.expr, Column):  # Table
            collector << self.quote_column_name(f'"{o.expr.name}"')
        else:
            collector << o.expr
        return collector

    def visit_Offset(self, o: Offset, collector: SQLString) -> SQLString:
        collector << "OFFSET "
        return self.visit(o.expr, collector)

    def visit_Limit(self, o: Offset, collector: SQLString) -> SQLString:
        collector << "LIMIT "
        return self.visit(o.expr, collector)

    def visit_Grouping(self, o: Grouping, collector: SQLString) -> SQLString:
        if isinstance(o.expr, Grouping):
            self.visit(o.expr, collector)
        else:
            collector << "("
            self.visit(o.expr, collector) << ")"
        return collector

    def visit_In(self, o: In, collector: SQLString) -> SQLString:
        attr, values = o.left, o.right
        if is_array(values):
            if not values:
                # Todo: values = [ value.unboundable() for value in values if value.__getattribute__('unboundable', None) is not None ]
                pass

            return collector << "1=0"

        self.visit(attr, collector) << " IN ("
        self.visit(values, collector) << ")"
        return collector

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

    def visit_Table(self, o: Table, collector: SQLString):
        # if issubclass(o.__class__, Node):  # other Node
        #     self.visit(o, collector)
        if issubclass(o.__class__, Table):  # Table
            collector << self.quote_table_name(f'"{o.name}"')
        if o.alias:
            collector << " " << self.quote_table_name(f'"{o.table_alias}"')
        return collector

    def visit_Offset(self, o: Offset, collector: SQLString):
        collector << "OFFSET "
        self.visit(o.expr, collector)

    def visit_Limit(self, o: Limit, collector: SQLString):
        collector << "LIMIT "
        self.visit(o.expr, collector)

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

    def visit_DeleteStatement(self, o: DeleteStatement, collector: SQLString) -> SQLString:
        o = self.prepare_delete_statement(o)
        if self.has_join_sources(o):
            collector << "DELETE "
            self.visit(o.relation.left, collector)
            collector << " FROM "
        else:
            collector << "DELETE FROM "

        collector = self.visit(o.relation, collector)

        self.collect_nodes_for(o.wheres, collector, " WHERE ", " AND ")
        self.collect_nodes_for(o.orders, collector, " ORDER BY ")
        self.maybe_visit(o.limit, collector)

        return collector

    def prepare_update_statement(self, o: TreeStatement) -> TreeStatement:
        if o.key and (self.has_limit_or_offset_or_orders(o) or self.has_join_sources(o)):
            stmt = copy.deepcopy(o)
            stmt.limit = None
            stmt.offset = None
            stmt.orders = []
            columns = Grouping(o.key)
            stmt.wheres = [In(columns, [self.build_subselect(o.key, o)])]
            if self.has_join_sources(o):
                stmt.relation = o.relation.left
            if o.groups:
                stmt.groups = o.groups
            if o.havings:
                stmt.havings = o.havings
            return stmt
        return o
    prepare_delete_statement = prepare_update_statement

    def collect_nodes_for(self, nodes, collector: SQLString, spacer, connector: str =", "):
        if nodes:
            collector << spacer
            self.inject_join(nodes, collector, connector)

    def inject_join(self, lst: [], collector: SQLString, join_str: str) -> SQLString:
        for i, x in enumerate(lst):
            if i != 0:
                collector << join_str
            collector = self.visit(x, collector)
        return collector
    def build_subselect(self, key, o) -> SelectStatement:
        stmt = SelectStatement()
        core = stmt.cores[0]
        core.froms = o.relation
        core.wheres = o.wheres
        core.projections = [key]
        if o.groups:
            core.groups = o.groups
        if o.havings:
            core.havings = o.havings
        stmt.limit = o.limit
        stmt.offset = o.offset
        stmt.orders = o.orders
        return stmt

    def has_join_sources(self, o):
        return isinstance(o.relation, JoinSource) and not o.relation.right.empty()

    def has_limit_or_offset_or_orders(self, o):
        return o.limit or o.offset or not o.orders.empty()
