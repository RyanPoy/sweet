from datetime import date, datetime
from decimal import Decimal
from typing import List, Tuple, TypeAlias, TYPE_CHECKING, Union, get_args, get_type_hints

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
    return f'{str_B()}, {str_V()}, List, Tuple'
