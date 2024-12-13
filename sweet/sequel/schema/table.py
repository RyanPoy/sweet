from functools import cached_property

from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.utils import DBDataType


class Table:

    def __init__(self, name: str) -> None:
        self.name = name

    @cached_property
    def name_quoted(self) -> str:
        return f'"{self.name}"'

    def insert(self, *values: DBDataType) -> InsertStatement:
        return InsertStatement().into(self).insert(*values)