from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal

from typing import Self, Tuple, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

# 基本数据类型，在sql中，有 str, int, float, Decimal, bool, date, datetime, list, tuple, None, bytes

Basic = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]

# Value 用在 set column = Value
Value1 = Union[Basic, 'Fn', 'Name']

BasicType = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]
ValueType = [BasicType, 'Fn', 'Name']


@dataclass
class Value:
    v: Basic | 'Fn' | 'Name'


class Values:

    def __init__(self, *args: ValueType) -> None:
        self.vs: Tuple[Value, ...] = args

    def is_empty(self):
        return self.vs is None or len(self.vs) == 0

    def __len__(self) -> int:
        return len(self.vs)

    def __eq__(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.vs == other.vs
