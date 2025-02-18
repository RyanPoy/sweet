from dataclasses import dataclass, field
from typing import List


@dataclass
class Column:
    pass


@dataclass
class Columns:
    data: List[Column] = field(init=False, default_factory=list)

    def append(self, col: Column):
        self.data.append(col)


