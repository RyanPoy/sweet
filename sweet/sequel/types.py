from __future__ import annotations

from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal
from typing import List, Tuple, TypeAlias, TYPE_CHECKING, Union

from sweet import utils
from sweet.sequel import quoting

if TYPE_CHECKING:
    from sweet.sequel.terms.name_fn import Name, Fn

B: TypeAlias = Union[int, float, str, Decimal, bool, datetime, date, None, bytes]
K: TypeAlias = Union['Fn', 'Name']
V: TypeAlias = Union[B, K, List, Tuple]


def is_B(o) -> bool:
    return isinstance(o, B)


def is_K(o) -> bool:
    from sweet.sequel.terms.name_fn import Name, Fn
    return isinstance(o, (Fn, Name))


def is_V(o) -> bool:
    return is_B(o) or is_K(o) or isinstance(o, (list, tuple))


def str_B() -> str:
    return 'int, float, str, Decimal, bool, datetime, date, None, bytes'


def str_K() -> str:
    return 'Fn, Name'


def str_V() -> str:
    return f'{str_B()}, {str_K()}, List, Tuple'


# Basic data type
RawType: TypeAlias = Union[None, bool, str, bytes, int, float, Decimal, date, datetime]


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
            return f"'{quoting.qs(self.data)}'"
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
ArrayType: TypeAlias = Union[Tuple, List]


@dataclass
class Array:
    data: List

    def __post_init__(self) -> None:
        self.data = self._normalize()

    def length(self) -> int:
        return len(self.data)

    def valid_for_between(self) -> bool:
        return self.length() == 2

    def _normalize(self):
        from sweet.sequel.terms.name_fn import Name, Fn
        def _(vs, lst: List) -> List:
            for x in vs:
                if isinstance(x, RawType):
                    lst.append(Raw(x))
                elif isinstance(x, (Fn, Name)):
                    lst.append(x)
                elif isinstance(x, ArrayType):
                    lst.append(_(x, []))
            return lst

        return _(self.data, [])
