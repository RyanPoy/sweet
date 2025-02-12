from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal
from typing import List, Tuple, TypeAlias, TYPE_CHECKING, Union, get_args, get_type_hints

from sweet import utils
from sweet.sequel import quoting

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

B: TypeAlias = Union[int, float, str, Decimal, bool, datetime, date, None, bytes]
K: TypeAlias = Union['Fn', 'Name']
V: TypeAlias = Union[B, K, List, Tuple]


def is_B(o) -> bool:
    return isinstance(o, B)


def is_K(o) -> bool:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name
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

# Array
ArrayType: TypeAlias = Union[Tuple, List]


@dataclass
class Raw:
    value: RawType

    def __post_init__(self):
        if not isinstance(self.value, RawType):
            raise TypeError(f"Raw initialize accept {RawType}, but got '{self.value.__class__.__name__}'")

    def quote(self):
        if self.value is None:
            return "NULL"

        tp = type(self.value)
        if tp == str:  # Todos: if tp in (str, ActiveSupport::Multibyte::Chars):
            return f"'{quoting.qs(self.value)}'"
        if tp == bool:
            return '1' if self.value is True else '0'
        if tp in (Decimal, int, float):  # BigDecimals need to be put in a non-normalized form and quoted.
            return str(self.value)
        if tp == datetime:
            return f"'{utils.datetime2str(self.value)}'"
        if tp == date:
            return f"'{utils.date2str(self.value)}'"
        if tp == bytes:
            return f"'{utils.binary2str(self.value)}'"
