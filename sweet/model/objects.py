from typing import List, Self, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.model import Model

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms.binary import Binary, Q


class Objects:

    def __init__(self, model_class: 'Model') -> None:
        self.model_class = model_class
        self.binary = None
        self._execute_func = None

    def filter(self, **kwargs) -> Self:
        binary = Q(**kwargs)
        if self.binary is not None:
            binary = binary & self.binary
        return self._copy_with(binary)

    def _copy_with(self, binary: Binary) -> Self:
        objs = self.__class__(self.model_class)
        objs.binary = binary
        return objs

    def all(self) -> Self:
        self._execute_func = self.__all
        return self

    def __all(self) -> List:
        stmt = SelectStatement()
        stmt.from_(self.model_class.table.name)
        stmt.where(self.binary)
        return []

    def first(self) -> 'Model':
        stmt = SelectStatement().limit(1)
        stmt.from_(self.model_class.table.name)
        stmt.where(self.binary)
        return self.model_class()

    def last(self) -> 'Model':
        stmt = SelectStatement()
        stmt.from_(self.model_class.table.name)
        stmt.where(self.binary)
        return self.model_class()

    def sql(self):
        if self._execute_func == self.__all:
            pass
        else:
            raise Exception("Unable to infer the execution method. Please call all(), first(), or last() first.")
