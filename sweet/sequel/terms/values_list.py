from typing import Self

from sweet.sequel.terms import Term
from sweet.utils import DBDataType


class ValuesList(Term):
    """
    vs = Values(1, "lily", 20)
    vs.append(2, "lucy", 32)
    vs.append((3, "jimy", 15), (4, "abc", 8))
    """
    def __init__(self, *vs: DBDataType) -> None:
        super().__init__()

        self.data = []
        if vs:
            self.data.append(vs)

    def is_empty(self):
        return False if self.data else True

    def append(self, rows: [[DBDataType]]) -> Self:
        if not rows:
            return self

        len0 = len(rows[0])
        for row in rows:
            if len0 != len(row):
                raise ValueError("Inconsistent row length")
        for row in rows:
            self.data.append(row)
        return self
