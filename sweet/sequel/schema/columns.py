from functools import cached_property


class Column:

    def __init__(self, name: str, table: "Table" = None) -> None:
        self.name = name
        self.table = None

    @cached_property
    def name_quoted(self) -> str:
        return f'"{self.name}"'
