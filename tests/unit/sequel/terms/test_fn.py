
from sweet.sequel import Operator
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Count, Name, Sum
from sweet.sequel.terms.literal import STAR


def test_count__star(visitors):
    func = Count(STAR)
    assert visitors.sqlite.sql(func) == 'COUNT(*)'
    assert visitors.mysql.sql(func) == 'COUNT(*)'
    assert visitors.pg.sql(func) == 'COUNT(*)'


def test_count__str(visitors):
    func = Count(Name("bar"))
    assert visitors.mysql.sql(func) == 'COUNT(`bar`)'
    assert visitors.sqlite.sql(func) == 'COUNT("bar")'
    assert visitors.pg.sql(func) == 'COUNT("bar")'


def test_count__Name(visitors):
    func = Count(Name("bar"))
    assert visitors.mysql.sql(func) == 'COUNT(`bar`)'
    assert visitors.sqlite.sql(func) == 'COUNT("bar")'
    assert visitors.pg.sql(func) == 'COUNT("bar")'


def test_count__as(visitors):
    func = Count(STAR).as_("foo")
    assert visitors.mysql.sql(func) == 'COUNT(*) AS `foo`'
    assert visitors.sqlite.sql(func) == 'COUNT(*) AS "foo"'
    assert visitors.pg.sql(func) == 'COUNT(*) AS "foo"'


def test_count_distinct(visitors):
    func = Count(Name("foo")).distinct()
    assert visitors.mysql.sql(func) == 'COUNT(DISTINCT `foo`)'
    assert visitors.sqlite.sql(func) == 'COUNT(DISTINCT "foo")'
    assert visitors.pg.sql(func) == 'COUNT(DISTINCT "foo")'


def test_sum_distinct(visitors):
    func = Sum(Name("foo")).distinct()
    assert visitors.mysql.sql(func) == 'SUM(DISTINCT `foo`)'
    assert visitors.sqlite.sql(func) == 'SUM(DISTINCT "foo")'
    assert visitors.pg.sql(func) == 'SUM(DISTINCT "foo")'


def test_gt(visitors):
    f = Sum(Name("foo"))
    assert f.gt(1) == Binary(f, Operator.GT, 1), f.gt(1)


def test_gt_Name(visitors):
    f = Sum(Name("foo"))
    n = Name("age")
    assert f.gt(n) == Binary(f, Operator.GT, n), f.gt(n)


def test_not_gt(visitors):
    f = Sum(Name("foo"))
    assert f.not_gt(1) == Binary(f, Operator.NOT_GT, 1), f.not_gt(1)


def test_gte(visitors):
    f = Sum(Name("foo"))
    assert f.gte(1) == Binary(f, Operator.GTE, 1), f.gte(1)


def test_not_gte(visitors):
    f = Sum(Name("foo"))
    assert f.not_gte(1) == Binary(f, Operator.NOT_GTE, 1), f.not_gte(1)


def test_lt(visitors):
    f = Sum(Name("foo"))
    assert f.lt(1) == Binary(f, Operator.LT, 1), f.lt(1)


def test_not_lt(visitors):
    f = Sum(Name("foo"))
    assert f.not_lt(1) == Binary(f, Operator.NOT_LT, 1), f.not_lt(1)


def test_lte(visitors):
    f = Sum(Name("foo"))
    assert f.lte(1) == Binary(f, Operator.LTE, 1), f.lte(1)


def test_not_lte(visitors):
    f = Sum(Name("foo"))
    assert f.not_lte(1) == Binary(f, Operator.NOT_LTE, 1), f.not_lte(1)


def test_eq(visitors):
    f = Sum(Name("foo"))
    assert f.eq(1) == Binary(f, Operator.EQ, 1), f.eq(1)


def test_not_eq(visitors):
    f = Sum(Name("foo"))
    assert f.not_eq(1) == Binary(f, Operator.NOT_EQ, 1), f.not_eq(1)


def test_and(visitors):
    func = Sum(Name("foo")).not_eq(1) & Sum(Name("foo")).lt(100)
    assert visitors.mysql.sql(func) == 'SUM(`foo`) <> 1 AND SUM(`foo`) < 100'
    assert visitors.sqlite.sql(func) == 'SUM("foo") <> 1 AND SUM("foo") < 100'
    assert visitors.pg.sql(func) == 'SUM("foo") <> 1 AND SUM("foo") < 100'


def test_or(visitors):
    func = Sum(Name("foo")).not_eq(1) | Sum(Name("foo")).lt(100)
    assert visitors.mysql.sql(func) == 'SUM(`foo`) <> 1 OR SUM(`foo`) < 100'
    assert visitors.sqlite.sql(func) == 'SUM("foo") <> 1 OR SUM("foo") < 100'
    assert visitors.pg.sql(func) == 'SUM("foo") <> 1 OR SUM("foo") < 100'


def test_and_or_nesting(visitors):
    func = Sum(Name("foo")).not_eq(1) & (Sum(Name("foo")).lt(100) | Sum(Name("bar")).gt(20))
    assert visitors.mysql.sql(func) == 'SUM(`foo`) <> 1 AND (SUM(`foo`) < 100 OR SUM(`bar`) > 20)'
    assert visitors.sqlite.sql(func) == 'SUM("foo") <> 1 AND (SUM("foo") < 100 OR SUM("bar") > 20)'
    assert visitors.pg.sql(func) == 'SUM("foo") <> 1 AND (SUM("foo") < 100 OR SUM("bar") > 20)'
