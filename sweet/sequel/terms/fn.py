from __future__ import annotations

import copy
from dataclasses import dataclass
from typing import List, Self, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.sequel.terms.binary import Binary

from sweet.sequel import Operator
from sweet.sequel.terms import literal
from sweet.sequel.terms.name import Name
from sweet.sequel.types import K, V, is_K
from sweet.utils import is_array


@dataclass
class Fn:
    name: str
    columns: List[K | literal] = None
    alias: str = None
    _distinct: bool = False

    def __post_init__(self) -> None:
        if self.columns is None:
            self.columns = []

    def column(self, *column_names: Union[K | str]) -> Self:
        for c in column_names:
            if c == '*' or c == literal.STAR:
                self.columns.append(literal.STAR)
            elif isinstance(c, literal.Literal):
                self.columns.append(c)
            elif is_K(c):
                self.columns.append(c.rm_alias())
            elif isinstance(c, str):
                self.columns.append(Name(c))
            else:
                raise ValueError(f"Support Name and Literal，but got[{c.__class__}] ")
        return self

    def is_distinct(self) -> bool:
        return self._distinct

    def distinct(self) -> Self:
        self._distinct = True
        return self

    def as_(self, name: str) -> Self:
        self.alias = name
        return self

    def eq(self, v: V) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS, v)
        elif is_array(v):
            return self._binary(Operator.IN, v)
        else:
            return self._binary(Operator.EQ, v)

    def not_eq(self, v: V) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS_NOT, v)
        elif is_array(v):
            return self._binary(Operator.NOT_IN, v)
        else:
            return self._binary(Operator.NOT_EQ, v)

    def gt(self, v: V) -> 'Binary':
        return self._binary(Operator.GT, v)

    def not_gt(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_GT, v)

    def gte(self, v: V) -> 'Binary':
        return self._binary(Operator.GTE, v)

    def not_gte(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_GTE, v)

    def lt(self, v: V) -> 'Binary':
        return self._binary(Operator.LT, v)

    def not_lt(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LT, v)

    def lte(self, v: V) -> 'Binary':
        return self._binary(Operator.LTE, v)

    def not_lte(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LTE, v)

    def like(self, v: V) -> 'Binary':
        return self._binary(Operator.LIKE, v)

    def not_like(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LIKE, v)

    def between(self, v: Sequence[V]) -> 'Binary':
        return self._binary(Operator.BETWEEN, v)

    def not_between(self, v: Sequence[V]) -> 'Binary':
        return self._binary(Operator.NOT_BETWEEN, v)

    def regex(self, v: str) -> 'Binary':
        return self._binary(Operator.REGEX, v)

    def not_regex(self, v: str) -> 'Binary':
        return self._binary(Operator.NOT_REGEX, v)

    def _binary(self, op, v) -> 'Binary':
        from sweet.sequel.terms.binary import Binary
        return Binary(self, op, v)


count = lambda *columns: Fn("COUNT").column(*columns)
sum = lambda *columns: Fn("SUM").column(*columns)
avg = lambda *columns: Fn("AVERAGE").column(*columns)
sqrt = lambda *columns: Fn("SQRT").column(*columns)
