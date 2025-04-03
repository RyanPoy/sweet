import pytest
from sweet.sequel.terms.binary import Q


def test_combine_empty_copy():
    assert Q(x=1) == Q(x=1) | Q()
    assert Q(x=1) == Q() | Q(x=1)
    assert Q(x=1) == Q(x=1) & Q()
    assert Q(x=1) == Q() & Q(x=1)
    assert Q() == Q() & Q()
    assert Q() == Q() | Q()


def test_equal(visitors):
    assert Q() == Q()
    assert Q(pk=(1, 2)) == Q(pk=(1, 2))
    assert Q(pk=(1, 2)) == Q(pk=[1, 2])


def test_combine_not_q_object(visitors):
    with pytest.raises(TypeError):
        Q(x=1) | object()
    with pytest.raises(TypeError, match="Binary AND operate with an other Binary instance, but got a object instance."):
        Q(x=1) & object()


def test_basic(visitors):
    q = Q(price="discounted_price")
    assert visitors.mysql.sql(q) == """`price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """"price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """"price" = 'discounted_price'"""


def test_or(visitors):
    q = Q(price__gt="discounted_price") | Q(price="discounted_price")
    assert visitors.mysql.sql(q) == """`price` > 'discounted_price' OR `price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """"price" > 'discounted_price' OR "price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """"price" > 'discounted_price' OR "price" = 'discounted_price'"""

    q = ~Q(price__gt="discounted_price") | ~Q(price="discounted_price")
    assert visitors.mysql.sql(q) == """NOT `price` > 'discounted_price' OR NOT `price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """NOT "price" > 'discounted_price' OR NOT "price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """NOT "price" > 'discounted_price' OR NOT "price" = 'discounted_price'"""


def test_and(visitors):
    q = Q(price__gt="discounted_price") & Q(price="discounted_price")
    assert visitors.mysql.sql(q) == """`price` > 'discounted_price' AND `price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """"price" > 'discounted_price' AND "price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """"price" > 'discounted_price' AND "price" = 'discounted_price'"""

    q = ~Q(price__gt="discounted_price") & ~Q(price="discounted_price")
    assert visitors.mysql.sql(q) == """NOT `price` > 'discounted_price' AND NOT `price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """NOT "price" > 'discounted_price' AND NOT "price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """NOT "price" > 'discounted_price' AND NOT "price" = 'discounted_price'"""


def test_deconstruct_multiple_kwargs(visitors):
    q = Q(price__gt="discounted_price", price="discounted_price")
    assert visitors.mysql.sql(q) == """`price` > 'discounted_price' AND `price` = 'discounted_price'"""
    assert visitors.sqlite.sql(q) == """"price" > 'discounted_price' AND "price" = 'discounted_price'"""
    assert visitors.pg.sql(q) == """"price" > 'discounted_price' AND "price" = 'discounted_price'"""


def test_complex_logic(visitors):
    q = (Q(price__gte="discounted_price") | ~Q(price="discounted_price")) & ~(Q(age__lte=20) | ~Q(name="abc"))
    assert visitors.mysql.sql(q) == """(`price` >= 'discounted_price' OR NOT `price` = 'discounted_price') AND NOT (`age` <= 20 OR NOT `name` = 'abc')"""
    assert visitors.sqlite.sql(q) == """("price" >= 'discounted_price' OR NOT "price" = 'discounted_price') AND NOT ("age" <= 20 OR NOT "name" = 'abc')"""
    assert visitors.pg.sql(q) == """("price" >= 'discounted_price' OR NOT "price" = 'discounted_price') AND NOT ("age" <= 20 OR NOT "name" = 'abc')"""

