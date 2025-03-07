from typing import List

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms.binary import Binary, Q


class Objects:

    def __init__(self, model_class) -> None:
        self.model_class = model_class
        self.binary = None

    def filter(self, **kwargs):
        binary = Q(**kwargs)
        if self.binary is not None:
            binary = binary & self.binary
        return self._copy_with(binary)

    def _copy_with(self, binary: Binary):
        objs = self.__class__(self.model_class)
        objs.binary = binary
        return objs

    def all(self) -> List:
        stmt = SelectStatement()
        stmt.where(self.binary)
        return