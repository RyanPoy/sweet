import copy
from typing import Callable

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.fn import Fn
from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.lock import Lock
from sweet.sequel.terms.order import OrderClause
from sweet.sequel.terms.pair import Pair, Operator
from sweet.sequel.terms.name import Name
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.value import Regexp, Value
from sweet.sequel.terms.values_list import ValuesList
from sweet.sequel.quoting import quote, quote_name, quote_condition, quote_value
from sweet.sequel.terms.where import Filter, Having, On, Where


class Visitor:
    visit_methods_dict = {}
    qchar = '"'

    def quote_column_name(self, name: str) -> str:
        return quote_name(name, self.qchar)

    def visit_Name(self, n: Name, sql: SQLCollector) -> SQLCollector:
        if n.schema_name:
            sql << self.quote_column_name(f"{n.schema_name}.{n.value}")
        else:
            sql << self.quote_column_name(n.value)
        if n.alias:
            sql << " AS " << self.quote_column_name(n.alias)
        return sql

    def visit_Literal(self, l: Literal, sql: SQLCollector) -> SQLCollector:
        return sql << l.v

    def visit_Fn(self, f: Fn, sql: SQLCollector) -> SQLCollector:
        if f.chain:
            sql << "("
        sql << f.name
        if f.parentheses:
            sql << "("
        else:
            sql << " "
        if f.nesting:
            self.visit_Fn(f.nesting, sql)
        for i, column in enumerate(f.columns):
            if i != 0: sql << ", "
            self.visit(column, sql)
        if f.parentheses:
            sql << ")"
        for i, pair in enumerate(f.cmp_pairs):
            sql << f" {pair[0]} "
            self.visit(pair[1], sql)
        for i, (logic_op, child) in enumerate(f.chain):
            sql << f" {str(logic_op)} "
            self.visit_Fn(child, sql)
        if f.alias:
            sql << " AS "
            self.visit(f.alias, sql)
        if f.chain:
            sql << ")"
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

    def visit_Q(self, q: Q, sql: SQLCollector) -> SQLCollector:
        if q.invert:
            sql << "NOT "
        if q.condition:
            self.visit(q.condition, sql)
        if q.children:
            sql << "("
            for i, c in enumerate(q.children):
                if i != 0: sql << f" {str(q.logic_op)} "
                self.visit_Q(c, sql)
            sql << ")"
        return sql

    def visit_Binary(self, b: Binary, sql: SQLCollector) -> SQLCollector:
        if isinstance(b.key, Name):
            self.visit(b.key, sql)
        else:
            sql << self.quote_column_name(b.key)
        sql << f" {b.op} "
        if b.op == Operator.BETWEEN or b.op == Operator.NOT_BETWEEN:
            if isinstance(b.value[0], Name):
                self.visit_Name(b.value[0].rm_alias(), sql)
            else:
                sql << quote_condition(b.value[0])
            sql << " AND "
            if isinstance(b.value[1], Name):
                self.visit_Name(b.value[1].rm_alias(), sql)
            else:
                sql << quote_condition(b.value[1])
        else:
            if isinstance(b.value, Name):
                self.visit(b.value.rm_alias(), sql)
            else:
                sql << quote_condition(b.value)
        return sql

    def visit_Pair(self, p: Pair, sql: SQLCollector) -> SQLCollector:
        def deal_name_value(v: Name):
            if v.alias:
                new_value = copy.deepcopy(v)
                new_value.alias = ""
                self.visit_Name(new_value, sql)
            else:
                self.visit_Name(v, sql)

        if isinstance(p.field, Name):
            self.visit_Name(p.field, sql)
        else:
            sql << self.quote_column_name(p.field)

        sql << f" {str(p.operator)} "
        if p.operator == Operator.BETWEEN or p.operator == Operator.NOT_BETWEEN:
            if isinstance(p.value[0], Name):
                deal_name_value(p.value[0])
            else:
                sql << quote_condition(p.value[0])
            sql << " AND "
            if isinstance(p.value[1], Name):
                deal_name_value(p.value[1])
            else:
                sql << quote_condition(p.value[1])
        else:
            if isinstance(p.value, Name):
                deal_name_value(p.value)
            else:
                sql << quote_condition(p.value)
        return sql

    def visit_Value(self, v: Value, sql: SQLCollector) -> SQLCollector:
        return sql << quote(v.v)

    def visit_Regexp(self, r: Regexp, sql: SQLCollector) -> SQLCollector:
        return sql << "REGEX" << quote(r.v)

    def visit_ValuesList(self, values: ValuesList, sql: SQLCollector) -> SQLCollector:
        for i, vs in enumerate(values.data):
            if i != 0: sql << ", "
            sql << "(" << ', '.join([quote_value(v) for v in vs]) << ")"
        return sql

    def visit_Where(self, where: Where, sql: SQLCollector) -> SQLCollector:
        return self._visit_Filter("WHERE", where, sql)

    def visit_Having(self, having: Having, sql: SQLCollector) -> SQLCollector:
        return self._visit_Filter("HAVING", having, sql)

    def visit_On(self, on: On, sql: SQLCollector) -> SQLCollector:
        return self._visit_Filter("ON", on, sql)

    def _visit_Filter(self, scope: str, filter: Filter, sql: SQLCollector) -> SQLCollector:
        if not filter.empty():
            sql << f" {scope} "
            for i, q in enumerate(filter.filters):
                if i != 0: sql << f" AND "
                self.visit(q, sql)
        return sql

    def visit_InsertStatement(self, stmt: InsertStatement, sql: SQLCollector) -> SQLCollector:
        self.visit(stmt.insert_or_update, sql)
        sql << f" INTO "
        sql = self.visit(stmt.table_name, sql)
        if stmt.columns:
            sql << " ("
            for i, c in enumerate(stmt.columns):
                if i != 0: sql << ", "
                self.visit(c, sql)
            sql << ")"
        sql << " VALUES "
        self.visit(stmt.values, sql)
        return sql

    def visit_DeleteStatement(self, stmt: DeleteStatement, sql: SQLCollector) -> SQLCollector:
        sql << "DELETE FROM "
        self.visit(stmt.table_name, sql)
        self.visit_Where(stmt.where_clause, sql)
        return sql

    def visit_UpdateStatement(self, stmt: UpdateStatement, sql: SQLCollector) -> SQLCollector:
        if not stmt.sets:
            return sql

        sql << f"UPDATE "
        sql = self.visit(stmt.table_name, sql)
        if stmt.sets:
            sql << " SET "
            i = 0
            for k, v in stmt.sets.items():
                if i != 0: sql << ", "
                sql << f"{self.quote_column_name(k)} = {quote_value(v)}"
                i += 1
        self.visit_Where(stmt.where_clause, sql)
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
            self.visit(stmt._distinct, sql)
            sql << " "

        if not stmt.columns:
            sql << "*"
        else:
            for i, c in enumerate(stmt.columns):
                if i != 0: sql << ", "
                self.visit(c, sql)

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
            self.visit_On(stmt.on_clause, sql)

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

        self.visit_Where(stmt.where_clause, sql)

        if stmt.groups:
            sql << " GROUP BY "
            for i, c in enumerate(stmt.groups):
                if i != 0: sql << ", "
                self.visit(c, sql)

        if not stmt.having_clause.empty():
            self.visit_Having(stmt.having_clause, sql)

        if stmt.orders:
            sql << " ORDER BY "
            for i, order in enumerate(stmt.orders):
                self.visit(order, sql)

        if stmt._limit:  sql << f" LIMIT {stmt._limit}"
        if stmt._offset: sql << f" OFFSET {stmt._offset}"

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
