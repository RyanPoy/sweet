from dataclasses import dataclass, field
from typing import Self

from sweet.sequel.terms.name_fn import Name


@dataclass
class Returnings:
    _data: dict = field(init=False, default_factory=dict)

    def append(self, *cols: Name) -> Self:
        for c in cols:
            self._data[c] = 1
        return self

    def __iter__(self):
        return iter(self._data.keys())

    def is_empty(self):
        return len(self._data) == 0

    def clear(self) -> Self:
        self._data.clear()
        return self
