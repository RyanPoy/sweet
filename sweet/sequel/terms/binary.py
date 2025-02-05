from typing import TYPE_CHECKING

from sweet.sequel import Operator
from sweet.utils import DBDataType

if TYPE_CHECKING:
    from sweet.sequel.terms.name import Name


class Binary:

    def __init__(self, op: Operator, key, value: DBDataType | 'Name' = None):
        self.key: Name | str = key
        self.value: Name | DBDataType = value
        self.op: Operator = op

