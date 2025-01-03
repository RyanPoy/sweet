from typing import Self

from sweet.sequel.schema.table import Table
from sweet.sequel.statements import Statement
from sweet.sequel.terms import literal
from sweet.sequel.terms.alias import Alias
from sweet.sequel.terms.name import IndexName, Name, TableName
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class SelectStatement(Statement):
    """
    SELECT
      ├── Columns
      ├── From
      │   └── table
      ├── Where
      │   └── Condition
      │       ├── column
      │       └── operator
      │       └── value
      ├── Group By
      │   ├── columns
      ├── Where
      │   └── Condition
      │       ├── column
      │       ├── operator
      │       └── value
      └── Order By
          ├── columns
          └── DESC / ASC
    """

    def __init__(self):
        super().__init__()

        self.tables = []
        self.columns = []
        self._distinct = None
        self._limit = 0
        self._offset = 0
        self.wheres = []
        self.force_indexes = []
        self.use_indexes = []
        self.lock = None
        self.join_tables = []
        self.ons = []

        self.parent = None

    def from_(self, table: TableName | Alias | Self) -> Self:
        return self.__from_or_join(self.tables, table)

    def select(self, *columns: Name | Alias | DBDataType) -> Self:
        for c in columns:
            if isinstance(c, (Name, Alias)):
                self.columns.append(c)
            elif c == '*':
                self.columns.append(literal.STAR)
            else:
                self.columns.append(Value(c))

        return self

    def force_index(self, *indexes: IndexName) -> Self:
        self.force_indexes.extend(indexes)
        return self

    def use_index(self, *indexes: IndexName) -> Self:
        self.use_indexes.extend(indexes)
        return self

    def distinct(self) -> Self:
        self._distinct = literal.DISTINCT
        return self

    def is_distinct_required(self) -> bool:
        return self._distinct == literal.DISTINCT

    def for_update(self, share: bool = False, nowait: bool = False, skip: bool = False) -> Self:
        if not share and not nowait and not skip:
            self.lock = literal.FOR_UPDATE
        elif share:
            self.lock = literal.FOR_UPDATE_SHARE
        elif nowait:
            self.lock = literal.FOR_UPDATE_NOWAIT
        elif skip:
            self.lock = literal.FOR_UPDATE_SKIP_LOCKED
        return self

    def is_locked(self) -> bool:
        return self.lock is not None

    def limit(self, limit) -> Self:
        self._limit = limit
        return self

    def offset(self, offset) -> Self:
        self._offset = offset
        return self

    def where(self, *qs: Q, **kwargs) -> Self:
        """
        Add filtering conditions for the SELECT statement.

        :param qs: `Q` objects represents filter conditions.
        :param kwargs: keyword arguments for creating filter conditions. (e.g., `id=1`)
        :return: The current UpdateStatement instance
        """
        return self.__where_or_on(self.wheres, *qs, **kwargs)

    def join(self, table: TableName | Alias | Self) -> Self:
        return self.__from_or_join(self.join_tables, table)

    def __from_or_join(self, cs, table: TableName | Alias | Self) -> Self:
        tp = type(table)
        if tp is TableName:
            self.__check_repetitive_tables(table, cs)
        elif tp is Alias:
            cs.append(table)
        elif tp is SelectStatement:
            if self.__check_circular_reference(table):
                raise ValueError("Circular reference detected in subquery nesting.")
            cs.append(table)
            table.parent = self  # set parent SelectStatement
        else:
            raise ValueError(f"from_ method only accepts Table, Alias, or Query as arguments. But got {tp}")
        return self

    def __check_circular_reference(self, ss) -> bool:
        current = self
        while current:
            if current is ss:
                return True
            current = current.parent
        return False

    def __check_repetitive_tables(self, table, tables) -> Self:
        for t in tables:  # check for repetitive tables
            if t.name == table.name:
                return self
        tables.append(table)
        return self

    def on(self, *qs: Q, **kwargs) -> Self:
        return self.__where_or_on(self.ons, *qs, **kwargs)

    def __where_or_on(self, cs, *qs: Q, **kwargs) -> Self:
        for q in qs:
            if not q.is_empty():
                cs.append(q)
        if kwargs:
            cs.append(Q(**kwargs))
        return self
