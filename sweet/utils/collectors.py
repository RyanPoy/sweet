import copy
from typing import Self


class SQLString:
    def __init__(self, s: str = None):
        self.bind_index = 1
        self._parts = []
        self.retryable = False
        if s:
            self._parts.append(s)

    def add_binds(self, binds: []) -> Self:
        end = self.bind_index + len(binds)
        self << ", ".join([ f"#{x}" for x in range(self.bind_index, end) ])
        self.bind_index = end
        return self

    def add_bind(self, bind):
        self << f"#{self.bind_index}"
        self.bind_index += 1
        return self

    @property
    def value(self) -> str:
        return "".join(self._parts)

    def __lshift__(self, item):
        if not isinstance(item, str):
            raise TypeError(f"Unsupported type for <<: {type(item)}")
        self._parts.append(item)
        return self

    def __copy__(self):
        return copy.deepcopy(self)

    def __str__(self):
        return self.value
