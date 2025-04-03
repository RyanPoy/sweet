from datetime import date, datetime
from decimal import Decimal

from sweet.sequel.terms.values import Values
from sweet.sequel.types import Array


def test_values():
    d1 = datetime(2024, 12, 13, 11, 9, 28, 547992)
    d2 = datetime(2024, 12, 13, 11, 9, 28, 547992)
    d3 = datetime(2024, 12, 13, 11, 9, 28, 547992)

    vs = Values([1, "lucy", 30, d1])
    assert vs.data == [Array([1, "lucy", 30, d1])]

    vs.append([2, "lily", 20, d2])
    assert vs.data == [Array([1, "lucy", 30, d1]), Array([2, "lily", 20, d2])]

    vs.append((3, "jim", 12, d3), (4, "noname", 9, d1))
    assert vs.data == [
        Array([1, "lucy", 30, d1]), Array([2, "lily", 20, d2]),
        Array([3, "jim", 12, d3]), Array([4, "noname", 9, d1])
    ]


def test_sql__none(visitors):
    vs = Values()
    assert visitors.mysql.sql(vs) == "(NULL)"
    assert visitors.sqlite.sql(vs) == "(NULL)"
    assert visitors.pg.sql(vs) == "(NULL)"


def test_sql__str_and_bytes(visitors):
    vs = Values(["lucy", b"hello, world"])
    assert visitors.mysql.sql(vs) == "('lucy', 'hello, world')"
    assert visitors.sqlite.sql(vs) == "('lucy', 'hello, world')"
    assert visitors.pg.sql(vs) == "('lucy', 'hello, world')"


def test_sql__bool_and_number(visitors):
    vs = Values([True, Decimal(10.000005), 30, 24.7])
    assert visitors.mysql.sql(vs) == "(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)"
    assert visitors.sqlite.sql(vs) == "(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)"
    assert visitors.pg.sql(vs) == "(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)"


def test_sql__date_and_datetime(visitors):
    vs = Values([
        datetime(2024, 12, 13, 11, 9, 28, 547992),
        date(2024, 12, 13)
    ])
    assert visitors.mysql.sql(vs) == "('2024-12-13 11:09:28', '2024-12-13')"
    assert visitors.sqlite.sql(vs) == "('2024-12-13 11:09:28', '2024-12-13')"
    assert visitors.pg.sql(vs) == "('2024-12-13 11:09:28', '2024-12-13')"


def test_sql__tuple_and_list(visitors):
    vs = Values(("list1", None, 1), ("tuple2", None, 2.1))
    assert visitors.mysql.sql(vs) == "('list1', NULL, 1), ('tuple2', NULL, 2.1)"
    assert visitors.sqlite.sql(vs) == "('list1', NULL, 1), ('tuple2', NULL, 2.1)"
    assert visitors.pg.sql(vs) == "('list1', NULL, 1), ('tuple2', NULL, 2.1)"
