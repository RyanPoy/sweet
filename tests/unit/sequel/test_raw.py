import pytest
from datetime import datetime
from decimal import Decimal

from sweet.sequel.terms.name_fn import Sum, Name
from sweet.sequel.types import Array, Raw


def test__basic_raw_init_error(visitors):
    with pytest.raises(TypeError,
                       match="Raw initialize accept None | bool | str | bytes | int | float | decimal.Decimal | datetime.date | datetime.datetime, but got 'object'"):
        Raw(object())


def test__basic_raw_quote():
    assert Raw(None).quote() == "NULL"
    assert Raw(True).quote() == "1"
    assert Raw(False).quote() == "0"
    assert Raw("jim").quote() == "'jim'"
    assert Raw(b"abcef").quote() == "'abcef'"
    assert Raw(1).quote() == "1"
    assert Raw(1.5).quote() == "1.5"
    assert Raw(Decimal(12.75)).quote() == "12.75"

    s = '2025-02-13'
    day = datetime.strptime(s, "%Y-%m-%d").date()
    assert Raw(day).quote() == "'2025-02-13'"

    s = '2025-02-13 10:03:20'
    dt = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
    assert Raw(dt).quote() == "'2025-02-13 10:03:20'"


def test__basic_raw_sql(visitors):
    assert visitors.mysql.sql(Raw(None)) == "NULL"
    assert visitors.sqlite.sql(Raw(None)) == "NULL"
    assert visitors.pg.sql(Raw(None)) == "NULL"

    assert visitors.mysql.sql(Raw(True)) == "1"
    assert visitors.sqlite.sql(Raw(True)) == "1"
    assert visitors.pg.sql(Raw(True)) == "1"

    assert visitors.mysql.sql(Raw(False)) == "0"
    assert visitors.sqlite.sql(Raw(False)) == "0"
    assert visitors.pg.sql(Raw(False)) == "0"

    assert visitors.mysql.sql(Raw("jim")) == "'jim'"
    assert visitors.sqlite.sql(Raw("jim")) == "'jim'"
    assert visitors.pg.sql(Raw("jim")) == "'jim'"

    assert visitors.mysql.sql(Raw(b"abcef")) == "'abcef'"
    assert visitors.sqlite.sql(Raw(b"abcef")) == "'abcef'"
    assert visitors.pg.sql(Raw(b"abcef")) == "'abcef'"

    assert visitors.mysql.sql(Raw(1)) == "1"
    assert visitors.sqlite.sql(Raw(1)) == "1"
    assert visitors.pg.sql(Raw(1)) == "1"

    assert visitors.mysql.sql(Raw(1.5)) == "1.5"
    assert visitors.sqlite.sql(Raw(1.5)) == "1.5"
    assert visitors.pg.sql(Raw(1.5)) == "1.5"

    assert visitors.mysql.sql(Raw(Decimal(12.75))) == "12.75"
    assert visitors.sqlite.sql(Raw(Decimal(12.75))) == "12.75"
    assert visitors.pg.sql(Raw(Decimal(12.75))) == "12.75"

    s = '2025-02-13'
    day = datetime.strptime(s, "%Y-%m-%d").date()
    assert visitors.mysql.sql(Raw(day)) == "'2025-02-13'"
    assert visitors.sqlite.sql(Raw(day)) == "'2025-02-13'"
    assert visitors.pg.sql(Raw(day)) == "'2025-02-13'"

    s = '2025-02-13 10:03:20'
    dt = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
    assert visitors.mysql.sql(Raw(dt)) == "'2025-02-13 10:03:20'"
    assert visitors.sqlite.sql(Raw(dt)) == "'2025-02-13 10:03:20'"
    assert visitors.pg.sql(Raw(dt)) == "'2025-02-13 10:03:20'"


def test__array_raw_init():
    fn = Sum("age")
    name = Name("username")
    s = '2025-02-13 10:03:20'
    now = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
    array = Array([
        1, "1", now, fn, name, [
            1, "1", now, fn, name, [
                1, "1", now, fn, name]
        ]
    ])
    assert array.data == [
        Raw(1), Raw("1"), Raw(now), fn, name, Array([
            Raw(1), Raw("1"), Raw(now), fn, name, Array([
                Raw(1), Raw("1"), Raw(now), fn, name])
        ])
    ]


def test__array_raw_sql(visitors):
    fn = Sum("age")
    name = Name("username")
    s = '2025-02-13 10:03:20'
    now = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
    array = Array([1, "1", now, fn, name, [1, "1", now, fn, name, [1, "1", now, fn, name]]])
    assert visitors.mysql.sql(array) == \
           "(1, '1', '2025-02-13 10:03:20', SUM(`age`), `username`, (1, '1', '2025-02-13 10:03:20', SUM(`age`), `username`, (1, '1', '2025-02-13 10:03:20', SUM(`age`), `username`)))"
    assert visitors.sqlite.sql(array) == \
           "(1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\")))"
    assert visitors.pg.sql(array) == \
           "(1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\")))"
