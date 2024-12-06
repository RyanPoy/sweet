from abc import ABC

from sweet.sequel import SqlLiteral, quoting
from sweet.sequel.nodes.insert_statement import InsertStatement
from sweet.sequel.nodes.node import Node
from sweet.sequel.nodes.values_list import ValuesList
from sweet.sequel.table import Table
from sweet.sequel.visitor.visitor import Visitor
from sweet.utils.collectors import SQLString


class ToSql(Visitor, ABC):

    def quote(self, value: SqlLiteral | str) -> object:
        return value if isinstance(value, SqlLiteral) else value

    def quote_column_name(self, name: SqlLiteral | str) -> str:
        return name.value if isinstance(name, SqlLiteral) else f'"{name}"'

    def quote_table_name(self, name: SqlLiteral | str) -> str:
        return name.value if isinstance(name, SqlLiteral) else f'"{name}"'

    def maybe_visit(self, thing, collector: SQLString) -> SQLString:
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
                collector << self.quote_column_name(c.__class__.__name__)
            collector << ")"

        if o.values:
            self.maybe_visit(o.values, collector)
        elif o.select:
            self.maybe_visit(o.select, collector)
        return collector

    def visit_Table(self, o: Node | Table, collector):
        if issubclass(o.__class__, Node):  # other Node
            self.visit(o, collector)
        else:  # Table
            collector << self.quote_table_name(o.name)
        if o.alias:
            collector << " " << self.quote_table_name(o.table_alias)
        return collector

        # if not o.insert_list:
        #     return "", []
        #
        # cols = o.insert_list[0].keys()
        # if len(o.insert_list) > 1:
        #     for r in o.insert_list:
        #         if r.keys() != cols:
        #             raise Exception("multiple insert only support same columns")
        #
        # begin, values_sql, params = 1, [], []
        # for r in o.insert_list:
        #     lst, begin = o.sequel.holder_parens_s(len(r), begin=begin)
        #     values_sql.append(lst)
        #     params.extend(r.values())
        #
        # cols_sql = qlist_parens(cols)
        # value_sql = ', '.join(values_sql)
        #
        # if o.returning_columns:
        #     returning_sql = '' if not o.returning_columns else qlist_parens(o.returning_columns)
        #     return f'INSERT INTO {o.tablename} {cols_sql} VALUES {value_sql} RETURNING {returning_sql}', params
        #
        # return f'INSERT INTO {o.tablename} {cols_sql} VALUES {value_sql}', params
