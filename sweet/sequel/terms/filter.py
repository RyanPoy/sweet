from dataclasses import dataclass, field
from typing import List, Self, Union

from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Fn, Name
from sweet.sequel.types import ArrayType, RawType


@dataclass
class Filter:
    filters: List[Binary] = field(init=False)

    def __post_init__(self) -> None:
        self.filters = []

    def add(self, *bs: Binary, **kwargs: Union[RawType, Name, Fn, ArrayType]) -> Self:
        for b in bs:
            if not isinstance(b, Binary):
                raise Exception(f"Filter add method only accepts the Binary type, but got {b.__class__.__name__}")
            self.filters.append(b)

        if kwargs:
            for k, v in kwargs.items():
                self.filters.append(Binary.parse(**{k: v}))

        return self

    def is_empty(self):
        return not self.filters or len(self.filters) == 0
