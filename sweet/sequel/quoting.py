from datetime import date, datetime
from decimal import Decimal

from sweet.sequel.terms.values import Value1
from sweet.utils import binary2str, date2str, datetime2str


def qs(s: str) -> str:
    return s.replace("\\", '\\\\').replace("'", "''")


def quote_value(value: Value1) -> str:
    return quote(value, "[", "]")


def quote_condition(value: Value1) -> str:
    return quote(value, "(", ")")


def quote_name(name: str, qchar: str) -> str:
    pointer = "."
    if "__" in name:
        name = name.replace("__", pointer)
    if pointer in name:
        return pointer.join([f'{qchar}{n}{qchar}' for n in name.split(pointer)])
    return f'{qchar}{name}{qchar}'


def quote(value: Value1, begin: str = "[", end: str = "]") -> str:
    """Quotes the column value to help prevent"""
    if value is None: return "NULL"

    tp = type(value)
    if tp == str:  # Todos: if tp in (str, ActiveSupport::Multibyte::Chars):
        return f"'{qs(value)}'"
    if tp == bool:
        return '1' if value is True else '0'
    if tp in (Decimal, int, float):  # BigDecimals need to be put in a non-normalized form and quoted.
        return str(value)
    if tp == datetime:
        return f"'{datetime2str(value)}'"
    if tp == date:
        return f"'{date2str(value)}'"
    if tp == bytes:
        return f"'{binary2str(value)}'"
    if tp in (tuple, list):
        return f"{begin}{', '.join([quote(v) for v in value])}{end}"
    raise TypeError(f"can't quote '{value.__class__.__name__}' type")

