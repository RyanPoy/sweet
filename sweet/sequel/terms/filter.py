from __future__ import annotations

from dataclasses import dataclass, field
from typing import Self

from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import ExtType
from sweet.sequel.types import ArrayType, RawType


@dataclass
class Filter:
    filters: list[Binary] = field(init=False)

    def __post_init__(self) -> None:
        self.filters = []

    def add(self, *bs: Binary, **kwargs: RawType | ExtType | ArrayType) -> Self:
        for b in bs:
            if b is None: continue
            if not isinstance(b, Binary):
                raise Exception(f"Filter add method only accepts the Binary type, but got {b.__class__.__name__}")
            self.filters.append(b)

        if kwargs:
            for k, v in kwargs.items():
                self.filters.append(Binary.parse(**{k: v}))

        return self

    def is_empty(self):
        return not self.filters or len(self.filters) == 0
