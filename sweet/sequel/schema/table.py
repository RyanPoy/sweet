from functools import cached_property


class Table:

    def __init__(self, name: str) -> None:
        self.name = name

    @cached_property
    def name_quoted(self) -> str:
        return f'"{self.name}"'
