from functools import cached_property

from sweet.sequel.schema.columns import Column
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.utils import DBDataType


class Table:

    def __init__(self, name: str, schema_name: str = None) -> None:
        self.name = name
        self.schema_name = schema_name

    @cached_property
    def name_quoted(self) -> str:
        if self.schema_name:
            return f'"{self.schema_name}"."{self.name}"'
        return f'"{self.name}"'

    def insert(self, *values: DBDataType) -> InsertStatement:
        return InsertStatement().into(self).insert(*values)

    def __setattr__(self, key, value):
        super().__setattr__(key, value)
        if isinstance(value, Column):
            if not value.table:
                value.table = self
