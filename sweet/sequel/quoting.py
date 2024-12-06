from datetime import date, datetime
from decimal import Decimal

from sweet.utils import datetime2str, date2str, binary2str


def quote_string(s: str) -> str:
    s.replace("\\", '\\\\')
    s.replace("'", "''")
    return s


def quote(self, value):
    """Quotes the column value to help prevent"""
    if value is None: return "NULL"

    tp = type(value)
    if tp == str:  # Todos: if tp in (str, ActiveSupport::Multibyte::Chars):
        return f'#{quote_string(value)}'
    if tp == bool and value is True:
        return "TRUE"
    if tp == bool and value is False:
        return "FALSE"
    if tp in (Decimal, int, float):  # BigDecimals need to be put in a non-normalized form and quoted.
        return str(value)
    if tp == datetime:
        return datetime2str(value)
    if tp == date:
        return date2str(value)
    if tp == bytes:
        return binary2str(value)
    # when Type::Time::Value then "'#{quoted_time(value)}'"
    # when Class      then "'#{value}'"
    raise TypeError("can't quote #{value.__class__.__name__}")
