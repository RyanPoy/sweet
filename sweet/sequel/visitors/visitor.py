from typing import Callable

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.schema.columns import Column
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms.condition import Condition, Operator
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.values import Values
from sweet.utils import DBDataType, quote, quote_for_values


class Visitor:

    visit_methods_dict = {}

    def quote_condition(self, value: DBDataType) -> str:
        return quote(value, "(", ")")

    def quote_column(self, column: Column | str):
        if isinstance(column, Column):
            return self.quote_column_name(column.name)
        return self.quote_column_name(column)

    def quote_column_name(self, name):
        pointer = "."
        if "__" in name:
            name = name.replace("__", pointer)
        if pointer in name:
            return pointer.join([ f"{n}" for n in name.split(pointer)])
        return f'"{name}"'

    def quote_values(self, values):
        return quote_for_values(values)

    def visit_Table(self, t: "Table", sql: SQLCollector) -> SQLCollector:
        sql << t.name_quoted
        return sql

    def visit_Alias(self, a: "Alias", sql: SQLCollector) -> SQLCollector:
        self.visit(a.target, sql)
        sql << " AS " << self.quote_column_name(a.as_str)
        return sql

    def visit_Column(self, c: Column, sql: SQLCollector) -> SQLCollector:
        sql << self.quote_column(c)
        return sql

    def visit_Q(self, q: Q, sql: SQLCollector) -> SQLCollector:
        if q.condition:
            self.visit_Condition(q.condition, sql)
        if q.children:
            sql << "("
            for i, c in enumerate(q.children):
                if i != 0: sql << f" {str(q.logic_op)} "
                self.visit_Q(c, sql)
            sql << ")"
        return sql

    def visit_Condition(self, c: Condition, sql: SQLCollector) -> SQLCollector:
        if c.operator == Operator.BETWEEN or c.operator == Operator.NOT_BETWEEN:
            sql << c.field_quoted << f" {str(c.operator)} {self.quote_values(c.value[0])} AND {self.quote_values(c.value[1])}"
        else:
            sql << f"{c.field_quoted} {str(c.operator)} {self.quote_condition(c.value)}"
        return sql

    def visit_Values(self, values: Values, sql: SQLCollector) -> SQLCollector:
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
        sql = self.visit(stmt.table, sql)
        if stmt._columns:
            sql << " (" << ", ".join([c.name_quoted for c in stmt._columns]) << ")"
        sql << " VALUES "
        self.visit(stmt.values, sql)
        return sql

    def visit_DeleteStatement(self, stmt: DeleteStatement, sql: SQLCollector) -> SQLCollector:
        sql << f"DELETE FROM {stmt.table.name_quoted}"
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
        sql = self.visit(stmt.table, sql)
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

    def visit_SelectStatement(self, stmt: SelectStatement, sql: SQLCollector) -> SQLCollector:
        sql << "SELECT "
        if stmt._distinct:
            sql << "DISTINCT "
        if not stmt.columns:
            sql << "*"
        else:
            for i, c in enumerate(stmt.columns):
                if i != 0: sql << ", "
                self.visit(c, sql)
        if stmt.table:
            sql << " FROM "
            sql = self.visit(stmt.table, sql)
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
