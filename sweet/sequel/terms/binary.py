from typing import Self, TYPE_CHECKING

from sweet.sequel import Operator
from sweet.utils import DBDataType, is_array

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

    def __eq__(self, other: Self) -> bool:
        if self.__class__ != other.__class__ or self.key != other.key or self.op != other.op:
            return False
        if is_array(self.value) and is_array(other.value):
            return list(self.value) == list(other.value)
        if not is_array(self.value) and not is_array(other.value):
            return self.value == other.value
        return False

    def __hash__(self) -> str:
        new_value = self.value
        if is_array(self.value):
            new_value = ''.join([ str(v) for v in self.value ])
        return f'{self.__class__}-{hash(self.key)}-{hash(new_value)}-{self.op}'
