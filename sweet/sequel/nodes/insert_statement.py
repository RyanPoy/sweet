import copy
from typing import Self

from sweet.sequel.nodes.node import Node
from sweet.sequel.table import Table
from sweet.utils import is_hash, is_array, qs


class InsertStatement(Node):

    def __init__(self):
        self.insert_list = []
        self.returning_columns = []
        self._is_replace = False

        self.table: Table = None
        self.columns: [str] = []
        self.values: [any] = []

    def __hash__(self):
        return hash(f"{''.join(self.columns)}-{''.join(self.values)}-{self.table}")

    def __eq__(self, other) -> bool:
        return (self.table == other.table
                and self.columns == other.columns
                and self.values == other.values)

    def clone(self) -> Self:
        return copy.deepcopy(self)

    def insert(self, records: list[dict] = None, **kwargs) -> Self:
        self._insert_or_replace(records, **kwargs)
        _is_replace = False
        return self

    def replace(self, records: list[dict] = None, **kwargs) -> Self:
        self._insert_or_replace(records, **kwargs)
        _is_replace = False
        return self

    def _insert_or_replace(self, records: list[dict] = None, **kwargs):
        if records:
            if is_hash(records):
                self.insert_list.append(records)
            elif is_array(records):
                self.insert_list.extend(records)
        if kwargs:
            self.insert_list.append(kwargs)

    def returning(self, *columns: list[str]) -> Self:
        self.returning_columns.extend(columns)
        return self
