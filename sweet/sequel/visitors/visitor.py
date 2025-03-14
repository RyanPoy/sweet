from __future__ import annotations

import copy
from typing import Callable
from sweet.sequel.collectors import SQLCollector
from sweet.sequel.logic import Logic
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms import literal
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name, Fn
from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.lock import Lock
from sweet.sequel.terms.order import OrderClause
from sweet.sequel.terms.values import Values
from sweet.sequel.terms.filter import Filter
from sweet.sequel.types import Array, ArrayType, Raw, RawType


class Visitor:
    visit_methods_dict = {}
    qchar = '"'

    def quote_column_name(self, name: str) -> str:
        pointer = "."
        name = name.replace("__", pointer)
        return '.'.join([ f'{self.qchar}{x}{self.qchar}' for x in name.split(pointer) ])

    def visit_Name(self, n: Name, sql: SQLCollector) -> SQLCollector:
        if n.schema_name:
            sql << self.quote_column_name(f"{n.schema_name}.{n.name}")
        else:
            sql << self.quote_column_name(n.name)
        if n.alias:
            sql << " AS " << self.quote_column_name(n.alias)
        return sql

    def visit_Literal(self, l: Literal, sql: SQLCollector) -> SQLCollector:
        return sql << l.v

    def visit_Fn(self, f: Fn, sql: SQLCollector) -> SQLCollector:
        sql << f.name << "("
        if f.is_distinct():
            self.visit_Literal(literal.DISTINCT, sql) << " "
        for i, c in enumerate(f.columns):
            if i != 0: sql << ", "
            if isinstance(c, (Raw, Name, Fn, Literal)):
                self.visit(c, sql)
            elif isinstance(c, RawType):
                self.visit_Raw(Raw(c), sql)
            else:
                raise ValueError(f'Fn column must be a RawType, Raw, Name, Fn, Literal，but got {c.__class__.__name__}')
        sql << ")"
        if f.alias:
            sql << " AS " << self.quote_column_name(f.alias)
        return sql

    def visit_Lock(self, l: Lock, sql: SQLCollector) -> SQLCollector:
        self.visit(l.prefix, sql)
        if l.ofs:
            sql << " OF "
            for i, n in enumerate(l.ofs):
                if i != 0: sql << ', '
                self.visit_Name(n, sql)
        if l.suffix:
            sql << " "
            self.visit(l.suffix, sql)
        return sql

    def visit_Binary(self, b: Binary, sql: SQLCollector) -> SQLCollector:
        def _visit_data_binary(node: Binary):
            if node.is_a_between_operation():
                self.visit(node.key, sql)
                sql << f" {node.op.invert() if node.inverted else node.op} "
                tuple_vs = node.value.data
                v = tuple_vs[0].rm_alias() if isinstance(tuple_vs[0], Name) else tuple_vs[0]
                self.visit(v, sql)
                sql << " AND "
                v = tuple_vs[1].rm_alias() if isinstance(tuple_vs[1], Name) else tuple_vs[1]
                self.visit(v, sql)
            elif node.is_a_special_operation():
                self.visit(node.key, sql)
                sql << f" {node.op.invert() if node.inverted else node.op} "
                self.visit(node.value, sql)
            else:
                if node.inverted:
                    sql << "NOT "
                self.visit(node.key, sql)
                sql << f" {node.op} "
                self.visit(node.value, sql)

        if b.for_logic():
            if b.inverted:
                sql << "NOT "
            if b.inverted and b.right is not None:
                sql << "("
            elif b.parent is not None and b.right is not None:
                if b.logic.priority() < b.parent.logic.priority():
                    sql << '('
            self.visit_Binary(b.left, sql)
            sql << f" {str(b.logic)} "
            self.visit_Binary(b.right, sql)
            if b.inverted and b.right is not None:
                sql << ")"
            elif b.parent is not None and b.right is not None: # 表示是一个logic关系的组合
                if b.logic.priority() < b.parent.logic.priority():
                    sql << ')'
        else:
            _visit_data_binary(b)
        return sql

    def visit_Raw(self, a: Raw, sql: SQLCollector) -> SQLCollector:
        return sql << a.quote()

    def visit_Array(self, a: Array, sql: SQLCollector, to_insert=False) -> SQLCollector:
        def _visit_seq(seq: ArrayType):
            sql << ("[" if to_insert else "(")
            for i, x in enumerate(seq):
                if i != 0:
                    sql << ", "
                if isinstance(x, RawType):
                    sql << Raw(x).quote()
                elif isinstance(x, Raw):
                    sql << x.quote()
                elif isinstance(x, Array):
                    self.visit_Array(x, sql)
                elif isinstance(x, ArrayType):
                    _visit_seq(x)
                elif isinstance(x, (Name, Fn)):
                    self.visit(x, sql)
                else:
                    raise TypeError(f"Got a unavailable element type {x.__class__.__name__}")
            sql << ("]" if to_insert else ")")
        _visit_seq(a.data)
        return sql

    def visit_Values(self, values: Values, sql: SQLCollector) -> SQLCollector:
        if values.is_empty():
            return sql << "(NULL)"
        for i, vs in enumerate(values.data):
            if i != 0: sql << ", "
            sql << "("
            for j, v in enumerate(vs.data):
                if j != 0: sql << ", "
                if isinstance(v, Array):
                    self.visit_Array(v, sql, True)
                else:
                    self.visit(v, sql)
            sql << ")"
        return sql

    def visit_Filter(self, filter: Filter, sql: SQLCollector) -> SQLCollector:
        if not filter.is_empty():
            for i, q in enumerate(filter.filters):
                if i != 0: sql << " AND "
                if q.for_logic() and q.logic.priority() < Logic.AND.priority():
                    sql << "("
                if isinstance(q.value, (Name, Fn)):
                    q.value = copy.deepcopy(q.value).rm_alias()
                self.visit(q, sql)
                if q.for_logic() and q.logic.priority() < Logic.AND.priority():
                    sql << ")"
        return sql

    def visit_InsertStatement(self, stmt: InsertStatement, sql: SQLCollector) -> SQLCollector:
        self.visit(stmt.insert_or_update, sql)
        sql << " INTO "
        sql = self.visit(stmt.table_name, sql)
        if stmt.columns:
            sql << " ("
            for i, c in enumerate(stmt.columns):
                if i != 0: sql << ", "
                self.visit(c, sql)
            sql << ")"
        sql << " VALUES "
        self.visit_Values(stmt.values, sql)
        return sql

    def visit_DeleteStatement(self, stmt: DeleteStatement, sql: SQLCollector) -> SQLCollector:
        sql << "DELETE FROM "
        self.visit(stmt.table_name, sql)
        if not stmt.where_clause.is_empty():
            sql << " WHERE "
            self.visit_Filter(stmt.where_clause, sql)
        return sql

    def visit_UpdateStatement(self, stmt: UpdateStatement, sql: SQLCollector) -> SQLCollector:
        if not stmt.sets:
            return sql

        sql << "UPDATE "
        sql = self.visit(stmt.table_name, sql)
        if stmt.sets:
            sql << " SET "
            for i, b in enumerate(stmt.sets):
                if i != 0: sql << ", "
                self.visit_Binary(b, sql)
        if not stmt.where_clause.is_empty():
            sql << ' WHERE '
            self.visit_Filter(stmt.where_clause, sql)

        return sql

    def visit_OrderClause(self, order: OrderClause, sql: SQLCollector) -> SQLCollector:
        for i, o in enumerate(order.orders):
            if i != 0: sql << ", "
            self.visit(o, sql)
        if order.sorted_in:
            sql << " " << str(order.sorted_in)
        return sql

    def visit_SelectStatement(self, stmt: SelectStatement, sql: SQLCollector, level=0) -> SQLCollector:
        sql << "SELECT "
        if stmt.is_distinct_required() and stmt.columns:
            self.visit(stmt.distinct_, sql)
            sql << " "

        if not stmt.columns:
            sql << "*"
        else:
            for i, c in enumerate(stmt.columns):
                if i != 0: sql << ", "
                if isinstance(c, (Raw, Name, Fn, Literal)):
                    self.visit(c, sql)
                elif isinstance(c, RawType):
                    self.visit_Raw(Raw(c), sql)
                else:
                    raise ValueError(f'SelectStatement column must be a Raw, RawType, Name, Fn, Literal, but got {c.__class__.__name__}')

        if stmt.tables:
            sql << " FROM "
            for i, table in enumerate(stmt.tables):
                if i != 0: sql << ", "
                if isinstance(table, SelectStatement):
                    sql << "("
                    self.visit_SelectStatement(table, sql, level + 1)
                    sql << f") AS sq{level}"
                else:
                    self.visit(table, sql)
        if stmt.join_tables:
            sql << " JOIN "
            for i, table in enumerate(stmt.join_tables):
                if i != 0: sql << ", "
                self.visit(table, sql)
            if stmt.on_clause:
                sql << " ON "
                self.visit_Filter(stmt.on_clause, sql)

        if stmt.force_indexes:
            sql << " FORCE INDEX ("
            for i, index in enumerate(stmt.force_indexes):
                if i != 0: sql << ", "
                self.visit(index, sql)
            sql << ")"

        if stmt.use_indexes:
            sql << " USE INDEX ("
            for i, index in enumerate(stmt.use_indexes):
                if i != 0: sql << ", "
                self.visit(index, sql)
            sql << ")"

        if not stmt.where_clause.is_empty():
            sql << ' WHERE '
            self.visit_Filter(stmt.where_clause, sql)

        if stmt.groups:
            sql << " GROUP BY "
            for i, c in enumerate(stmt.groups):
                if i != 0: sql << ", "
                self.visit(c, sql)

        if not stmt.having_clause.is_empty():
            sql << " HAVING "
            self.visit_Filter(stmt.having_clause, sql)

        if stmt.orders:
            sql << " ORDER BY "
            for i, order in enumerate(stmt.orders):
                self.visit(order, sql)

        if stmt.limit_number:  sql << f" LIMIT {stmt.limit_number}"
        if stmt.offset_number: sql << f" OFFSET {stmt.offset_number}"

        if stmt.lock:
            sql << " "
            self.visit(stmt.lock, sql)
        return sql

    def sql(self, o: any) -> str:
        return str(self.visit(o))

    def visit(self, o: any, sql: SQLCollector = None) -> SQLCollector:
        method = self.dispatch(o)
        if sql is None:
            sql = SQLCollector()
        return method(o, sql)

    def dispatch(self, o: any) -> Callable:
        methods = self.__class__.visit_methods_dict
        name = f'visit_{o.__class__.__name__}'
        if name in methods:
            return methods[name]
        method = self.__getattribute__(name)
        methods[name] = method
        return method

