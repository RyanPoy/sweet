from typing import Optional

from sweet.sequel.terms.name import TableName


class Statement:

    def __init__(self):
        self._table_name : Optional[TableName] = None

    @property
    def table_name(self) -> TableName:
        return self._table_name

    def sql(self, visitor: "Visitor") -> str:
        c = visitor.visit(self)
        return str(c)

