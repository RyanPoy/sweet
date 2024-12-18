from functools import cached_property
from sweet.sequel.terms.alias import Alias


class Column:

    def __init__(self, name: str, table: "Table" = None) -> None:
        self.name = name
        self.table = None
        self._as = None

    @cached_property
    def name_quoted(self) -> str:
        return f'"{self.name}"'

    def as_(self, alias: str) -> Alias:
        return Alias(self, alias)
