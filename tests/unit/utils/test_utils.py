import pytest
from datetime import date, datetime
from decimal import Decimal, InvalidOperation

from sweet.utils import str2date, str2datetime, to_bool, to_decimal, to_f, to_i


def test_to_i():
    assert 9 == to_i(9)
    assert 9 == to_i('9')
    pytest.raises(ValueError, to_i, '9p')


def test_to_f():
    assert 9.0, to_f(9)
    assert 9.0, to_f('9')
    pytest.raises(ValueError, to_f, '9p')


def test_to_decimal():
    d = Decimal(1)
    assert d == to_decimal(d)
    assert Decimal(9) == to_decimal(9)
    pytest.raises(InvalidOperation, to_decimal, '9c')


def test_to_bool_true():
    assert to_bool(1) is True
    assert to_bool(True) is True
    assert to_bool('T') is True
    assert to_bool('t') is True
    assert to_bool('true') is True
    assert to_bool('True') is True
    assert to_bool('yes') is True
    assert to_bool('Yes') is True
    assert to_bool('YES') is True


def test_to_bool_false():
    assert to_bool({'A': 'A'}) is False
    assert to_bool([1]) is False
    assert to_bool((2,)) is False

    assert to_bool('abc') is False
    assert to_bool('') is False
    assert to_bool('   ') is False
    assert to_bool(0) is False
    assert to_bool(False) is False
    assert to_bool('F') is False
    assert to_bool('f') is False
    assert to_bool('false') is False
    assert to_bool('FALSE') is False
    assert to_bool({}) is False
    assert to_bool([]) is False
    assert to_bool(()) is False
    assert to_bool('no') is False
    assert to_bool('No') is False
    assert to_bool('NO') is False


def test_str2date():
    assert date(2009, 10, 10), str2date('2009-10-10')
    assert date(2009, 10, 10), str2date('2009-10-10 10:10:10')


def test_str2datetime():
    assert datetime(2009, 10, 10, 10, 10, 10), str2datetime('2009-10-10 10:10:10')
    assert datetime(2009, 10, 10, 10, 10, 10, 100000), str2datetime('2009-10-10 10:10:10.100000')
    assert datetime(2009, 10, 10, 10, 10, 10, 100), str2datetime('2009-10-10 10:10:10.000100')
    assert datetime(2009, 10, 10, 0, 0, 0, 0), str2datetime('2009-10-10')

    assert datetime(2009, 10, 10, 10, 10, 10), str2datetime('2009/10/10 10:10:10')
    assert datetime(2009, 10, 10, 10, 10, 10, 100000), str2datetime('2009/10/10 10:10:10.100000')
    assert datetime(2009, 10, 10, 10, 10, 10, 100), str2datetime('2009/10/10 10:10:10.000100')
    assert datetime(2009, 10, 10, 0, 0, 0, 0), str2datetime('2009/10/10')


def test_str2date_when_empty_string():
    assert str2date('') is None
    assert str2date('  ') is None
    assert str2date(None) is None
