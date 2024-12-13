from typing import Self


class SQLCollector:

    def __init__(self) -> None:
        self.bind_index = 1
        self._parts = []

    def __lshift__(self, item) -> Self:
        if not isinstance(item, str):
            raise TypeError(f"Unsupported type for <<: {type(item)}")
        self._parts.append(item)
        return self

    def __str__(self) -> str:
        return "".join(self._parts)
