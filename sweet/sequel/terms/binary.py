from typing import Union, TYPE_CHECKING

from sweet.utils import DBDataType

if TYPE_CHECKING:
    from sweet.sequel.terms.name import Name


class Binary:

    def __init__(self, op: str, key, value: DBDataType | 'Name' = None):
        self.key: Name | str = key
        self.value: Name | DBDataType = value
        self.op: str = op

    def belongs_to_between(self):
        return self.op == 'BETWEEN' or self.op == 'NOT BETWEEN'
