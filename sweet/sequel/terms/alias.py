from typing import Union


class Alias:

    def __init__(self, target: Union["Column", "Table"], as_str: str):
        self.target = target
        self.as_str = as_str
