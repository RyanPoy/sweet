from typing import Callable

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.schema.columns import Column
from sweet.sequel.schema.table import Table
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms.alias import Alias
from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.pair import Pair, Operator
from sweet.sequel.terms.name import ColumnName, TableName
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.value import Value
from sweet.sequel.terms.values_list import ValuesList
from sweet.utils import DBDataType, quote, quote_for_values


class Visitor:

    visit_methods_dict = {}
    qchar = '"'

    def quote_condition(self, value: DBDataType) -> str:
        return quote(value, "(", ")")

    def quote_column(self, column: Column | str):
        if isinstance(column, Column):
            return self.quote_column_name(column.name)
        return self.quote_column_name(column)

    def _quote_table_name_or_column_name(self, name) -> str:
        pointer = "."
        if "__" in name:
            name = name.replace("__", pointer)
        if pointer in name:
            return pointer.join([ f'{self.qchar}{n}{self.qchar}' for n in name.split(pointer)])
        return f'{self.qchar}{name}{self.qchar}'
    quote_table_name = _quote_table_name_or_column_name
    quote_column_name = _quote_table_name_or_column_name

    def quote_values(self, values):
        return quote_for_values(values)

    def visit_Table(self, t: Table, sql: SQLCollector) -> SQLCollector:
        return sql << t.name_quoted

    def visit_TableName(self, n: TableName, sql: SQLCollector) -> SQLCollector:
        if n.schema_name:
            return sql << self.quote_table_name(f"{n.schema_name}.{n.value}")
        return sql << self.quote_table_name(n.value)

    def visit_ColumnName(self, n: ColumnName, sql: SQLCollector) -> SQLCollector:
        if n.schema_name:
            return sql << self.quote_column_name(f"{n.schema_name}.{n.value}")
        return sql << self.quote_column_name(n.value)

    def visit_IndexName(self, n: ColumnName, sql: SQLCollector) -> SQLCollector:
        if n.schema_name:
            return sql << self.quote_column_name(f"{n.schema_name}.{n.value}")
        return sql << self.quote_column_name(n.value)

    def visit_Alias(self, a: Alias, sql: SQLCollector) -> SQLCollector:
        self.visit(a.origin, sql)
        if a.target:
            sql << " AS "
            self.visit(a.target, sql)
        return sql

    def visit_Column(self, c: Column, sql: SQLCollector) -> SQLCollector:
        return sql << self.quote_column(c)

    def visit_Literal(self, l: Literal, sql: SQLCollector) -> SQLCollector:
        return sql << l.v

    def visit_Q(self, q: Q, sql: SQLCollector) -> SQLCollector:
        if q.condition:
            self.visit_Pair(q.condition, sql)
        if q.children:
            sql << "("
            for i, c in enumerate(q.children):
                if i != 0: sql << f" {str(q.logic_op)} "
                self.visit_Q(c, sql)
            sql << ")"
        return sql

    def visit_Pair(self, p: Pair, sql: SQLCollector) -> SQLCollector:
        if p.operator == Operator.BETWEEN or p.operator == Operator.NOT_BETWEEN:
            sql << self.quote_column_name(p.field) << f" {str(p.operator)} {self.quote_values(p.value[0])} AND {self.quote_values(p.value[1])}"
        else:
            sql << f"{self.quote_column_name(p.field)} {str(p.operator)} {self.quote_condition(p.value)}"
        return sql

    def visit_Value(self, v: Value, sql: SQLCollector) -> SQLCollector:
        return sql << quote(v.v)

    def visit_ValuesList(self, values: ValuesList, sql: SQLCollector) -> SQLCollector:
        for i, vs in enumerate(values.data):
            if i != 0: sql << ", "
            sql << "(" << ', '.join([ self.quote_values(v) for v in vs ]) << ")"
        return sql

    def visit_InsertStatement(self, stmt: InsertStatement, sql: SQLCollector) -> SQLCollector:
        if stmt.is_replace():
            sql << "REPLACE"
        elif stmt.is_ignore():
            sql << "INSERT IGNORE"
        else:
            sql << "INSERT"

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
        if stmt.wheres:
            sql << " WHERE "
            for i, w in enumerate(stmt.wheres):
                if i != 0: sql << f" AND "
                self.visit(w, sql)
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
                sql << f"{self.quote_column_name(k)} = {self.quote_values(v)}"
                i += 1
        if stmt.wheres:
            sql << " WHERE "
            for i, w in enumerate(stmt.wheres):
                if i != 0: sql << f" AND "
                self.visit(w, sql)
        return sql

    def visit_SelectStatement(self, stmt: SelectStatement, sql: SQLCollector, level=0) -> SQLCollector:
        sql << "SELECT "
        if stmt.is_distinct_required():
            self.visit(stmt._distinct, sql)
            sql << " "
        if not stmt.columns: sql << "*"

        for i, c in enumerate(stmt.columns):
            if i != 0: sql << ", "
            self.visit(c, sql)

        if stmt.tables:
            sql << " FROM "
            for i, table in enumerate(stmt.tables):
                if i != 0: sql << ", "
                if isinstance(table, SelectStatement):
                    sql << "("
                    self.visit_SelectStatement(table, sql, level+1)
                    sql << f") AS sq{level}"
                else:
                    self.visit(table, sql)

        if stmt.wheres:
            sql << " WHERE "
            for i, w in enumerate(stmt.wheres):
                if i != 0: sql << f" AND "
                self.visit(w, sql)

        if stmt._limit:  sql << f" LIMIT {stmt._limit}"
        if stmt._offset: sql << f" OFFSET {stmt._offset}"

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
        if stmt.lock:
            sql << " "
            self.visit(stmt.lock, sql)
        return sql

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
