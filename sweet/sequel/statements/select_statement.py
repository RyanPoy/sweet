from __future__ import annotations

from typing import List, Self, Union

from sweet.sequel.terms import literal
from sweet.sequel.terms.lock import Lock
from sweet.sequel.terms.name import Name
from sweet.sequel.terms.order import OrderClause, SortedIn
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.values import Value
from sweet.sequel.terms.where import Having, On, Where
from sweet.sequel.types import K, V, is_K


class SelectStatement:
    """
    SELECT
      ├── Columns (Name class)
      ├── From
      │   └── Table (Name class)
      ├── Where
      │   └── Condition
      │       ├── column
      │       └── operator
      │       └── value
      ├── Group By
      │   ├── Columns (Name class)
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
        self.tables = []
        self.columns: List[K | Value | literal.Literal] = []
        self.distinct_ = None
        self.limit_number = 0
        self.offset_number = 0
        self.where_clause: Where = Where()
        self.having_clause: Having = Having()
        self.force_indexes = []
        self.use_indexes = []
        self.lock = None
        self.join_tables = []
        self.on_clause : On = On()
        self.groups = []
        self.orders : [OrderClause] = []
        self.parent = None

    def from_(self, table: Name | Self) -> Self:
        return self.__from_or_join(self.tables, table)

    def select(self, *columns: Union[V | literal.Literal]) -> Self:
        for c in columns:
            if isinstance(c, str) and '*' == c:
                self.columns.append(literal.STAR)
            if is_K(c) or isinstance(c, literal.Literal):
                self.columns.append(c)
            else:
                self.columns.append(Value(c))
        return self

    def force_index(self, *indexes: Name) -> Self:
        self.force_indexes.extend(indexes)
        return self

    def use_index(self, *indexes: Name) -> Self:
        self.use_indexes.extend(indexes)
        return self

    def distinct(self) -> Self:
        self.distinct_ = literal.DISTINCT
        return self

    def is_distinct_required(self) -> bool:
        return self.distinct_ == literal.DISTINCT

    def for_update(self, share: bool = False, nowait: bool = False, skip: bool = False, of: (str,) = ()) -> Self:
        self.lock = Lock(share=share, nowait=nowait, skip=skip, of=of)
        return self

    def is_locked(self) -> bool:
        return self.lock is not None

    def limit(self, limit) -> Self:
        self.limit_number = limit
        return self

    def offset(self, offset) -> Self:
        self.offset_number = offset
        return self

    def where(self, *qs: Q, **kwargs) -> Self:
        """
        Add filtering conditions for the SELECT statement.

        :param qs: `Q` objects represents filter conditions.
        :param kwargs: keyword arguments for creating filter conditions. (e.g., `id=1`)
        :return: The current UpdateStatement instance
        """
        self.where_clause.append(*qs, **kwargs)
        return self

    def having(self, *qs: Q, **kwargs) -> Self:
        self.having_clause.append(*qs, **kwargs)
        return self

    def join(self, table: Name | Self) -> Self:
        return self.__from_or_join(self.join_tables, table)

    def on(self, *qs: Q, **kwargs) -> Self:
        self.on_clause.append(*qs, **kwargs)
        return self

    def group_by(self, *column_names: Name) -> Self:
        for c in column_names:
            if c.alias:
                self.groups.append(Name(c.alias))
            else:
                self.groups.append(c)
        return self

    def order_by(self, *column_names: Name, sorted_in: SortedIn = None) -> Self:
        order = OrderClause(*column_names, sorted_in=sorted_in)
        self.orders.append(order)
        return self

    def __from_or_join(self, cs, table: Name | Self) -> Self:
        tp = type(table)
        if tp is Name:
            self.__check_repetitive_tables(table, cs)
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
