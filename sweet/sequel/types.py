from __future__ import annotations

from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal
from typing import List, Tuple, TypeAlias
from sweet import utils

# Basic data type
RawType: TypeAlias = None | bool | str | bytes | int | float | Decimal | date | datetime


@dataclass
class Raw:
    data: RawType

    def __post_init__(self):
        if not isinstance(self.data, RawType):
            raise TypeError(f"Raw initialize accept {RawType}, but got '{self.data.__class__.__name__}'")

    def quote(self):
        if self.data is None:
            return "NULL"

        tp = type(self.data)
        if tp == str:  # Todos: if tp in (str, ActiveSupport::Multibyte::Chars):
            s = self.data.replace("\\", '\\\\').replace("'", "''")
            return f"'{s}'"
        if tp == bool:
            return '1' if self.data is True else '0'
        if tp in (Decimal, int, float):  # BigDecimals need to be put in a non-normalized form and quoted.
            return str(self.data)
        if tp == date:
            return f"'{utils.date2str(self.data)}'"
        if tp == datetime:
            return f"'{utils.datetime2str(self.data)}'"
        if tp == bytes:
            return f"'{utils.binary2str(self.data)}'"


# Array
ArrayType: TypeAlias = Tuple | List


@dataclass
class Array:
    data: List

    def __init__(self, data: List = None):
        self.data = self.__normalize(data)

    def length(self) -> int:
        return len(self.data)

    def valid_for_between(self) -> bool:
        return self.length() == 2

    def __normalize(self, vs: List) -> List:
        from sweet.sequel.terms.name_fn import Name, Fn
        lst = []
        for x in vs:
            if isinstance(x, RawType):
                lst.append(Raw(x))
            elif isinstance(x, ArrayType):
                lst.append(Array(x))
            elif isinstance(x, (Raw, Name, Fn, Array)):
                lst.append(x)
            else:
                raise TypeError(f"Array initialize only accepts List、Tuple、RawType，but got {x.__class__.__name__}")
        return lst
