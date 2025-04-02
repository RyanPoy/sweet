import pytest
from datetime import date, datetime, time
from decimal import Decimal

from sweet.model.columns import BinaryColumn, BooleanColumn, CharColumn, DateColumn, DatetimeColumn, DecimalColumn, FloatColumn, IntColumn, TextColumn, \
    TimeColumn
from sweet.model import Model


def test_unavailable_length():
    with pytest.raises(ValueError, match="The length must be greater than zero, but got -10"):
        CharColumn(length=-10)

    with pytest.raises(ValueError, match='The length must be greater than zero, but got 0'):
        TextColumn(length=0)

    with pytest.raises(ValueError):
        BinaryColumn(length=-10)


def test_name_is_not_none_when_defined_in_model():
    class Demo(Model):
        id = IntColumn()
        name = CharColumn("demo_name")

    assert 'demo_name' == Demo.table.columns.name.name
    assert 'id' == Demo.table.columns.id.name


def test_value():
    col = CharColumn()
    col.value = "abc"
    assert "abc" == col.value

    col = TextColumn()
    col.value = "abc"
    assert "abc" == col.value

    col = BinaryColumn()
    col.value = "abc"
    assert b"abc" == col.value

    col = IntColumn()
    col.value = 10
    assert 10 == col.value

    col = BooleanColumn()
    col.value = "y"
    assert col.value is True

    col.value = "n"
    assert col.value is False

    col = FloatColumn()
    col.value = " 12.33"
    assert 12.33 == col.value

    col = DecimalColumn()
    col.value = "12.45"
    assert Decimal("12.45") == col.value

    col = DateColumn()
    col.value = "2016-10-9"
    assert date(2016, 10, 9) == col.value

    col = DatetimeColumn()
    col.value = "2016-10-9 11:12:3"
    assert datetime(2016, 10, 9, 11, 12, 3) == col.value

    col = TimeColumn()
    col.value = "11:12:3"
    assert time(11, 12, 3) == col.value


def test_value_error():
    with pytest.raises(ValueError, match="Can't purify 19, it's a int type."):
        BinaryColumn().value = 19

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        IntColumn().value = "abc"

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        FloatColumn().value = "abc"

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        DecimalColumn().value = "abc"

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        DateColumn().value = "abc"

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        DatetimeColumn().value = "abc"

    with pytest.raises(ValueError, match="Can't purify abc, it's a str type."):
        TimeColumn().value = "abc"
