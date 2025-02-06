from typing import Self, TYPE_CHECKING

from sweet.sequel import Operator
from sweet.utils import DBDataType

if TYPE_CHECKING:
    from sweet.sequel.terms.name import Name


class Binary:

    def __init__(self, op: Operator, key, value: DBDataType | 'Name' = None) -> None:
        self.key: Name | str = key
        self.value: Name | DBDataType = value
        self.op: Operator = op

    def invert(self) -> Self:
        self.op = self.op.invert()
        return self
