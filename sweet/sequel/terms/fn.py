from __future__ import annotations

import copy
from dataclasses import dataclass
from typing import List, Self, Union

from sweet.sequel.terms.ext_key import ExtKey

from sweet.sequel.terms import literal
from sweet.sequel.terms.name import Name
from sweet.sequel.types import K, is_K


@dataclass
class Fn(ExtKey):
    columns: List[K | literal] = None
    _distinct: bool = False

    def __post_init__(self) -> None:
        if self.columns is None:
            self.columns = []

    def rm_alias(self) -> Self:
        instance = copy.copy(self)
        instance.alias = None
        return instance

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



count = lambda *columns: Fn("COUNT").column(*columns)
sum = lambda *columns: Fn("SUM").column(*columns)
avg = lambda *columns: Fn("AVERAGE").column(*columns)
sqrt = lambda *columns: Fn("SQRT").column(*columns)
