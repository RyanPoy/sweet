from datetime import date, datetime
from decimal import Decimal

from typing import Union, TYPE_CHECKING
if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name


# 基本数据类型，在sql中，有 str, int, float, Decimal, bool, date, datetime, list, tuple, None, bytes

Basic = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]

# Value 用在 set column = Value
Value1 = Union[Basic, 'Fn', 'Name']



