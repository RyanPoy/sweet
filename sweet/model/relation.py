from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.visitors.visitor import Visitor


class Relation:

    def __init__(self, select_statement: SelectStatement, sql_visitor: Visitor):
        self.stmt = select_statement
        self.visitor = sql_visitor
        self.__data = []

    def sql(self) -> str:
        return self.visitor.sql(self.stmt)

