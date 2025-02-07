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

RawType = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]
ValueType = [RawType, 'Fn', 'Name']


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


class ValuesList:
    """
    Represents a collection of rows to be used in SQL statements, such as INSERT or UPDATE.
    Each row is a tuple of value, and all rows must have consistent column length.

    Usage:
        # initialize with a single row
        vs = ValuesList(1, "lily", 20)

        # add a single row
        vs.append([(2, "lucy", 32)])

        # add multiple rows
        vs.append([ (3, "jimy", 15), (4, "abc", 8) ])
    """

    def __init__(self, *args: Values) -> None:
        self.data: [Values] = []
        self.append(*args)

    def is_empty(self):
        """
        Check if the ValuesList contains any rows.

        :return: True if the ValuesList is empty, otherwise False
        """
        return False if self.data else True

    def append(self, *args: Values) -> Self:
        """
        Append the row to the ValuesList. All rows must have consistent column length as existing rows.

        :param values: A list of tuples where each tuple represents a row to be added.
        :raises ValueError: If the rows have inconsistent length.
        :return: The current instance of ValuesList.
        """
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
