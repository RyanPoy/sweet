from typing import Union

from sweet.utils.inflection import *
from datetime import datetime, date
from decimal import Decimal
from queue import Queue
import time
import re

DBDataType = Union[int, float, str, Decimal, bool, date, datetime, list, tuple, None, bytes]

# data type transfer variables and functions
FALSE_VALUES = (None, '', 0, '0', 'f', 'F', 'false', 'FALSE', 'No', 'no', 'NO')
ISO_DATE = r'^(\d{4})-(\d{1,2})-(\d{1,2})$'
ISO_DATETIME = r'^(\d{4})-(\d{1,2})-(\d{1,2}) (\d{1,2}):(\d{1,2}):(\d{1,2})(\.\d+)?$'


def qs(s: str) -> str:
    return s.replace("\\", '\\\\').replace("'", "''")


def quote_value(value: DBDataType) -> str:
    return quote(value, "[", "]")


def quote_condition(value: DBDataType) -> str:
    return quote(value, "(", ")")


def quote(value: DBDataType, begin: str = "[", end: str = "]") -> str:
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


def to_bool(v):
    """ convert something to a boolean  """
    if not v:           return False
    if is_str(v):       v = v.strip()
    if v in FALSE_VALUES:   return False
    if not v:           return False
    return True


def to_i(v):
    if v is True:   return 1
    if v is False:  return 0
    try:
        return int(v)
    except:
        return 0


def to_f(v):
    try:
        return float(v)
    except:
        return 0.0


def to_decimal(v):
    if is_decimal(v):   return v
    try:
        return Decimal(v)
    except:
        return Decimal(0)


datetime2str = lambda dt, format='%Y-%m-%d %H:%M:%S': dt.strftime(format)
date2str = lambda d, format='%Y-%m-%d': d.strftime(format)

datetime2date = lambda dt: date(dt.year, dt.month, dt.day)
str2binary = lambda v: v.encode('utf-8')  # Used to convert from Strings to BLOBs
binary2str = lambda v: v.decode("utf-8")  # Used to convert from BLOBs to Strings

microseconds = lambda t: to_i((to_f(t) % 1) * 1000000)  # '0.123456' -> 123456; '1.123456' -> 123456


def str2datetime(s):
    def __fast_str2datetime(string):
        """ Doesn't handle time zones. """
        if re.match(ISO_DATETIME, string):
            subs = re.split(ISO_DATETIME, string)
            microsec = to_i((to_f(subs[7]) * 1000000))
            return datetime(to_i(subs[1]), to_i(subs[2]), to_i(subs[3]),
                            to_i(subs[4]), to_i(subs[5]), to_i(subs[6]),
                            microsec)
        return None

    def __fallback_str2datetime(string):
        try:
            d = str2date(string)
            return datetime(d.year, d.month, d.day)
        except:
            iso_string = string.strip()
            pos = iso_string.rfind('.')
            if pos != -1:
                iso_string[:pos]
                microsec = microseconds(iso_string[pos:])
            else:
                microsec = 0

            tm_struct, err = None, None
            for format_str in ['%Y-%m-%d %H:%M:%S', '%Y/%m/%d %H:%M:%S', '%Y-%m-%d', '%Y/%m/%d']:
                try:
                    tm_struct = time.strptime(string, format_str)
                    break
                except ValueError as ex:
                    err = ex
            if tm_struct is None:
                raise err
            return datetime(tm_struct.tm_year, tm_struct.tm_mon, tm_struct.tm_mday, tm_struct.tm_hour, tm_struct.tm_min,
                            tm_struct.tm_sec, microsec)

    if s is None:     return None
    if not is_str(s): return s
    if not s.strip(): return None
    return __fast_str2datetime(s) or __fallback_str2datetime(s)


def str2date(s):
    def __fast_str2date(string):
        if re.match(ISO_DATE, string):
            subs = re.split(ISO_DATE, string)
            return date(to_i(subs[1]), to_i(subs[2]), to_i(subs[3]))
        return None

    def __fallback_str2date(string):
        string = string.strip()
        string = string.split()[0]

        tm_struct, err = None, None
        for format_str in ['%Y-%m-%d', '%Y/%m/%d', '%Y-%m-%d %H:%M:%S', '%Y/%m/%d %H:%M:%S']:
            try:
                tm_struct = time.strptime(string, format_str)
                break
            except ValueError as ex:
                err = ex

        if tm_struct is None:
            raise err
        return date(tm_struct.tm_year, tm_struct.tm_mon, tm_struct.tm_mday)

    if s is None:           return None
    if is_datetime(s):      return datetime2date(s)
    if is_date(s):          return s
    if not is_str(s):       return s
    if not s.strip():       return None
    return __fast_str2date(s) or __fallback_str2date(s)


# check the object type functions
is_num = lambda obj: isinstance(obj, (int, float, Decimal))
is_blank_str = lambda obj: obj.strip() == ''
is_bytes = lambda obj: isinstance(obj, bytes)
is_str = lambda obj: isinstance(obj, str)
is_decimal = lambda obj: isinstance(obj, Decimal)
is_true = lambda obj: isinstance(obj, bool) and obj is True
is_false = lambda obj: isinstance(obj, bool) and obj is False
is_date = lambda obj: isinstance(obj, date)
is_datetime = lambda obj: isinstance(obj, datetime)
is_hash = lambda obj: isinstance(obj, dict)
is_array = lambda obj: isinstance(obj, (tuple, list))
is_set = lambda obj: isinstance(obj, set)


def replace_multiple(s: str, groups: [(str, str), ]) -> str:
    """

    :rtype: object
    """
    for g in groups:
        s.replace(g[0], g[1])
    return s


def xflatten(sequnce):
    for x in sequnce:
        if isinstance(x, (list, tuple)):
            for y in xflatten(x):
                yield y
        #         elif isinstance(x, Collection):
        #             for y in xflatten(x.items):
        #                 yield y
        else:
            yield x


def flatten(sequnce):
    lst = []
    for x in xflatten(sequnce):
        lst.append(x)
    return lst


def import_object(name):
    """
    the code copy from tornado

    Imports an object by name.

    import_object('x.y.z') is equivalent to 'from x.y import z'.

    >>> import tornado.escape
    >>> import_object('tornado.escape') is tornado.escape
    True
    >>> import_object('tornado.escape.utf8') is tornado.escape.utf8
    True
    """
    parts = name.split('.')
    obj = __import__('.'.join(parts[:-1]), None, None, [parts[-1]], 0)
    return getattr(obj, parts[-1])


class classproperty:
    """
    A decorate for class method. Use it like this:
    
    class Foo:
    
        __value = 10

        @classproperty
        def value(cls):
            cls.__value += 1
            return cls.__value
    
    for x in range(10):
        print Foo.value
    """

    def __init__(self, fget): self.fget = fget

    def __get__(self, owner_self, owner_cls): return self.fget(owner_cls)


class mydict(dict):

    def __getattr__(self, k):
        if k in self:
            return self[k]
        return super().__getattribute__(k)


class Q(Queue):
    def get(self, block=True, timeout=None):
        if self.qsize() <= 0:
            return None
        return super().get(block=block, timeout=timeout)
