from typing import Callable

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.terms.values import Values
from sweet.utils import quote


class Visitor:

    visit_methods_dict = {}

    def visit_Values(self, values: Values, sql: SQLCollector) -> SQLCollector:
        for i, vs in enumerate(values.data):
            if i != 0: sql << ", "
            sql << "(" << ', '.join([ quote(v) for v in vs ]) << ")"
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
