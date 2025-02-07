from datetime import date, datetime
from decimal import Decimal

from typing import Union, TYPE_CHECKING
if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name


Basic = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]
Value = Union[Basic, 'Fn', 'Name']
