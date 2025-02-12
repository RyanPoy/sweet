import unittest

from sweet.sequel import Operator
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Count, Name, Sum
from sweet.sequel.terms.literal import STAR
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestFn(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_count__star(self):
        func = Count(STAR)
        self.assertEqual('COUNT(*)', self.mysql.sql(func))
        self.assertEqual('COUNT(*)', self.sqlite.sql(func))
        self.assertEqual('COUNT(*)', self.pg.sql(func))

    def test_count__str(self):
        func = Count(Name("bar"))
        self.assertEqual('COUNT(`bar`)', self.mysql.sql(func))
        self.assertEqual('COUNT("bar")', self.sqlite.sql(func))
        self.assertEqual('COUNT("bar")', self.pg.sql(func))

    def test_count__Name(self):
        func = Count(Name("bar"))
        self.assertEqual('COUNT(`bar`)', self.mysql.sql(func))
        self.assertEqual('COUNT("bar")', self.sqlite.sql(func))
        self.assertEqual('COUNT("bar")', self.pg.sql(func))

    def test_count__as(self):
        func = Count(STAR).as_("foo")
        self.assertEqual('COUNT(*) AS `foo`', self.mysql.sql(func))
        self.assertEqual('COUNT(*) AS "foo"', self.sqlite.sql(func))
        self.assertEqual('COUNT(*) AS "foo"', self.pg.sql(func))

    def test_count_distinct(self):
        func = Count(Name("foo")).distinct()
        self.assertEqual('COUNT(DISTINCT `foo`)', self.mysql.sql(func))
        self.assertEqual('COUNT(DISTINCT "foo")', self.sqlite.sql(func))
        self.assertEqual('COUNT(DISTINCT "foo")', self.pg.sql(func))

    def test_sum_distinct(self):
        func = Sum(Name("foo")).distinct()
        self.assertEqual('SUM(DISTINCT `foo`)', self.mysql.sql(func))
        self.assertEqual('SUM(DISTINCT "foo")', self.sqlite.sql(func))
        self.assertEqual('SUM(DISTINCT "foo")', self.pg.sql(func))

    def test_gt(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.GT, 1), f.gt(1))

    def test_gt_Name(self):
        f = Sum(Name("foo"))
        n = Name("age")
        self.assertEqual(Binary(f, Operator.GT, n), f.gt(n))

    def test_not_gt(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.NOT_GT, 1), f.not_gt(1))

    def test_gte(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.GTE, 1), f.gte(1))

    def test_not_gte(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.NOT_GTE, 1), f.not_gte(1))

    def test_lt(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.LT, 1), f.lt(1))

    def test_not_lt(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.NOT_LT, 1), f.not_lt(1))

    def test_lte(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.LTE, 1), f.lte(1))

    def test_not_lte(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.NOT_LTE, 1), f.not_lte(1))

    def test_eq(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.EQ, 1), f.eq(1))

    def test_not_eq(self):
        f = Sum(Name("foo"))
        self.assertEqual(Binary(f, Operator.NOT_EQ, 1), f.not_eq(1))

    # def test_and(self):
    #     func = (Sum(Name("foo")) != 1) & (Sum(Name("foo")) < 100)
    #     self.assertEqual('(SUM(`foo`) <> 1 AND SUM(`foo`) < 100)', self.mysql.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 AND SUM("foo") < 100)', self.sqlite.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 AND SUM("foo") < 100)', self.pg.sql(func))
    #
    # def test_or(self):
    #     func = (Sum(Name("foo")) != 1) | (Sum(Name("foo")) < 100)
    #     self.assertEqual('(SUM(`foo`) <> 1 OR SUM(`foo`) < 100)', self.mysql.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 OR SUM("foo") < 100)', self.sqlite.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 OR SUM("foo") < 100)', self.pg.sql(func))
    #
    # def test_and_or_nesting(self):
    #     func = (Sum(Name("foo")) != 1) & ((Sum(Name("foo")) < 100) | (Sum(Name("bar")) > 20))
    #     self.assertEqual('(SUM(`foo`) <> 1 AND ((SUM(`foo`) < 100 OR SUM(`bar`) > 20)))', self.mysql.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 AND ((SUM("foo") < 100 OR SUM("bar") > 20)))', self.sqlite.sql(func))
    #     self.assertEqual('(SUM("foo") <> 1 AND ((SUM("foo") < 100 OR SUM("bar") > 20)))', self.pg.sql(func))


if __name__ == '__main__':
    unittest.main()
