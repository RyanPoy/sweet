#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from .inflection import Inflection
from datetime import datetime, date
from decimal import Decimal
import time
import re


camel_of = Inflection.hungarian_of
python_of = Inflection.pascal_of
singularize_of = Inflection.singularize_of
pluralize_of = Inflection.pluralize_of


# data type transfer variables and functions
FALSE_VALUES = (None, '', 0, '0', 'f', 'F', 'false', 'FALSE')
ISO_DATE = r'^(\d{4})-(\d\d)-(\d\d)$'
ISO_DATETIME = r'^(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)(\.\d+)?$'


def backtick(s):
    return '.'.join([ '`%s`' % x.strip() for x in s.split('.') if x and x.strip() ])
    
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
    try:            return int(v)
    except:         return 0


def to_f(v):
    try:    return float(v)
    except: return 0.0


def to_decimal(v):
    if is_decimal(v):   return v
    try:                return Decimal(v)
    except:             return Decimal(0)


datetime2str    = lambda dt, format='%Y-%m-%d %H:%M:%S': dt.strftime(format)
date2str        = lambda d, format='%Y-%m-%d': d.strftime(format)

datetime2date   = lambda dt: date(dt.year, dt.month, dt.day)
str2binary      = lambda v: v # Used to convert from Strings to BLOBs
binary2str      = lambda v: v # Used to convert from BLOBs to Strings

microseconds    = lambda t: to_i((to_f(t) % 1) * 1000000) # '0.123456' -> 123456; '1.123456' -> 123456
    

def str2datetime(s):
    if s is None:     return None
    if not is_str(s): return s
    if not s.strip(): return None
    return __fast_str2datetime(s) or __fallback_str2datetime(s)

def str2date(s):
    if s is None:           return None
    if is_datetime(s):      return datetime2date(s)
    if is_date(s):          return s
    if not is_str(s):       return s
    if not s.strip():       return None    
    return __fast_str2date(s) or __fallback_str2date(s)


def __fast_str2datetime(string):
    """ Doesn't handle time zones. """
    if re.match(ISO_DATETIME, string):
        subs = re.split(ISO_DATETIME, string)
        microsec = to_i((to_f(subs[7]) * 1000000))
        return datetime(to_i(subs[1]), to_i(subs[2]), to_i(subs[3]), 
                        to_i(subs[4]), to_i(subs[5]), to_i(subs[6]), 
                        microsec)
    return None


def __fast_str2date(string):
    if re.match(ISO_DATE, string):
        subs = re.split(ISO_DATE, string)
        return date(to_i(subs[1]), to_i(subs[2]), to_i(subs[3]))
    return None


def __fallback_str2date(string):
    string = string.strip()
    string = string.split()[0]
    tm_struct = time.strptime(string, '%Y-%m-%d')
    return date(tm_struct.tm_year, tm_struct.tm_mon, tm_struct.tm_mday)


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

        tm_struct = time.strptime(string, '%Y-%m-%d %H:%M:%S')
        return datetime(tm_struct.tm_year, tm_struct.tm_mon, tm_struct.tm_mday, tm_struct.tm_hour, tm_struct.tm_min, tm_struct.tm_sec, microsec)
    

# check the object type functions
is_num      = lambda obj: isinstance(obj, (int, long, float, Decimal))
is_blank_str = lambda obj: obj.strip() == ''
is_str      = lambda obj: isinstance(obj, (str, unicode))
is_decimal  = lambda obj: isinstance(obj, Decimal)

is_date     = lambda obj: isinstance(obj, date)
is_datetime = lambda obj: isinstance(obj, datetime)

is_hash     = lambda obj: isinstance(obj, dict)
is_array    = lambda obj: isinstance(obj, (tuple, list, set))


# other functions

def xflatten(sequnce):
    for x in sequnce:
        if isinstance(x, (list, tuple)):
            for y in xflatten(x):
                yield y
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
