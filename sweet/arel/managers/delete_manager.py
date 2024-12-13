from sweet import arel
from sweet.arel.managers.tree_manager import TreeManager
from sweet.arel.nodes.delete_statement import DeleteStatement
from sweet.arel.nodes.group import Group


class DeleteManager(TreeManager):

    def __init__(self):
        super().__init__()
        self.ast = DeleteStatement()

    def from_(self, relation):
        self.ast.relation = relation
        return self

    def group(self, columns):
        for column in columns:
            if isinstance(column, str):
                column = arel.sql(column)
            self.ast.groups.append(Group(column))
        return self

    def having(self, expr):
        self.ast.havings.append(expr)
        return self
