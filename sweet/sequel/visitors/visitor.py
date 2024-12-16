from typing import Callable

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.terms.condition import Condition, Operator
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.values import Values
from sweet.utils import quote_for_values, quote_for_condition


class Visitor:

    visit_methods_dict = {}

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
            sql << c.field_quoted << f" {str(c.operator)} {quote_for_condition(c.value[0])} AND {quote_for_condition(c.value[1])}"
        else:
            sql << f"{c.field_quoted} {str(c.operator)} {quote_for_condition(c.value)}"
        return sql

    def visit_Values(self, values: Values, sql: SQLCollector) -> SQLCollector:
        for i, vs in enumerate(values.data):
            if i != 0: sql << ", "
            sql << "(" << ', '.join([ quote_for_values(v) for v in vs ]) << ")"
        return sql

    def visit_InsertStatement(self, stmt: InsertStatement, sql: SQLCollector) -> SQLCollector:
        if stmt.is_replace():
            sql << "REPLACE"
        elif stmt.is_ignore():
            sql << "INSERT IGNORE"
        else:
            sql << "INSERT"

        sql << f" INTO {stmt.table.name_quoted}"
        if stmt._columns:
            sql << " (" << ", ".join([c.name_quoted for c in stmt._columns]) << ")"
        sql << " VALUES "
        self.visit(stmt.values, sql)
        return sql

    def visit_DeleteStatement(self, stmt: DeleteStatement, sql: SQLCollector) -> SQLCollector:
        sql << f"DELETE FROM {stmt.table.name_quoted}"
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
