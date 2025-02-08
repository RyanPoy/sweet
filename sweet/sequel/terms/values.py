from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal

from typing import Any, List, Self, Sequence, Tuple, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

# 基本数据类型，在sql中，有 str, int, float, Decimal, bool, date, datetime, list, tuple, None, bytes
Basic = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]
# Value 用在 set column = Value
Value1 = Union[Basic, 'Fn', 'Name']

RawType = Union[int, float, str, Decimal, bool, date, datetime, Sequence, None, bytes]
ValueType = Union[RawType, 'Fn', 'Name']


# @todo: Binary 里面应该使用 ValueType 和 Value，在处理binary的时候，如果Value有alias，应该把alias去掉，生成新的Value ？
# @todo: Q 里面应该使用 ValueType 和 Value
# @todo: Insert table(columns...) values() 这个地方的values方法，里面应该接受ValueType，内部转化为 ValuesList
# @todo: Update table set key=value 这个地方的key=value，应该使用ValueType，且内部需要把key=value转为Binary
# @todo: 在Values、ValuesList里面，Value里面的元素是 Sequence，那么用 '[]' 包裹；
#        在Binary里面的，Value里面的元素如果是Sequence，那么用 '()' 包裹；
# @todo: Visitor中的 visit_Value 要重构，里面要包装，不要直接用 value.v，而应该包装起来；
#        因此，对应的 quote_values_value(value: RawType) 也要重构，参数要由RawType变成Value类型

@dataclass
class Value:
    v : ValueType


class Values:

    def __init__(self, *args: ValueType) -> None:
        self.vs: Tuple[Value, ...] = tuple(Value(x) for x in args)

    def is_empty(self):
        return self.vs is None or len(self.vs) == 0

    def __len__(self) -> int:
        return len(self.vs)

    def __eq__(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.vs == other.vs


class ValuesList:

    def __init__(self, *args: Values) -> None:
        self.data: [Values] = []
        self.append(*args)

    def is_empty(self):
        return False if self.data else True

    def append(self, *args: Values) -> Self:
        args = [x for x in args if not x.is_empty()]
        self.check_values(*args)
        self.data.extend(args)
        return self

    def check_values(self, *args: Values) -> None:
        if not args:
            return
        len0 = len(args[0])
        for values in args:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")

        for values in self.data:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")
