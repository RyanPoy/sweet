from typing import Union


class Alias:

    def __init__(self, target: Union["Column", "Table", str], as_str: str):
        self.target = target
        self.as_str = as_str


def alias_of(s: str) -> Alias:
    return Alias(None, as_str=s)
