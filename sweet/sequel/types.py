from datetime import date, datetime
from decimal import Decimal
from typing import List, Tuple, TypeAlias, TYPE_CHECKING, Union, get_args, get_type_hints

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

B: TypeAlias = Union[int, float, str, Decimal, bool, datetime, date, None, bytes]
K: TypeAlias = Union['Fn', 'Name']
V: TypeAlias = Union[B, K, List, Tuple, List, Tuple, List, Tuple]


def is_B(v) -> bool:
    return isinstance(v, B)


def is_K(v) -> bool:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name
    return isinstance(v, (Fn, Name))
