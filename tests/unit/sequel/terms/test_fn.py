import unittest

from sweet.sequel.terms import fn, literal
from sweet.sequel.terms.name import Name
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestFn(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_count__star(self):
        func = fn.count(literal.STAR)
        self.assertEqual('COUNT(*)', self.mysql.sql(func))
        self.assertEqual('COUNT(*)', self.sqlite.sql(func))
        self.assertEqual('COUNT(*)', self.pg.sql(func))

    def test_count__str(self):
        func = fn.count(Name("bar"))
        self.assertEqual('COUNT(`bar`)', self.mysql.sql(func))
        self.assertEqual('COUNT("bar")', self.sqlite.sql(func))
        self.assertEqual('COUNT("bar")', self.pg.sql(func))

    def test_count__Name(self):
        func = fn.count(Name("bar"))
        self.assertEqual('COUNT(`bar`)', self.mysql.sql(func))
        self.assertEqual('COUNT("bar")', self.sqlite.sql(func))
        self.assertEqual('COUNT("bar")', self.pg.sql(func))

    def test_count__as(self):
        func = fn.count(literal.STAR).as_("foo")
        self.assertEqual('COUNT(*) AS `foo`', self.mysql.sql(func))
        self.assertEqual('COUNT(*) AS "foo"', self.sqlite.sql(func))
        self.assertEqual('COUNT(*) AS "foo"', self.pg.sql(func))

    def test_count_distinct(self):
        func = fn.count(Name("foo")).distinct()
        self.assertEqual('COUNT(DISTINCT `foo`)', self.mysql.sql(func))
        self.assertEqual('COUNT(DISTINCT "foo")', self.sqlite.sql(func))
        self.assertEqual('COUNT(DISTINCT "foo")', self.pg.sql(func))

    def test_sum_distinct(self):
        func = fn.sum(Name("foo")).distinct()
        self.assertEqual('SUM(DISTINCT `foo`)', self.mysql.sql(func))
        self.assertEqual('SUM(DISTINCT "foo")', self.sqlite.sql(func))
        self.assertEqual('SUM(DISTINCT "foo")', self.pg.sql(func))

    def test_sum__gt(self):
        func = fn.sum(Name("foo")) > 1
        self.assertEqual('SUM(`foo`) > 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") > 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") > 1', self.pg.sql(func))

    def test_sum__gt_Name(self):
        func = fn.sum(Name("foo")) > Name("age")
        self.assertEqual('SUM(`foo`) > `age`', self.mysql.sql(func))
        self.assertEqual('SUM("foo") > "age"', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") > "age"', self.pg.sql(func))

    def test_sum__gte(self):
        func = fn.sum(Name("foo")) >= 1
        self.assertEqual('SUM(`foo`) >= 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") >= 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") >= 1', self.pg.sql(func))

    def test_sum__lt(self):
        func = fn.sum(Name("foo")) < 1
        self.assertEqual('SUM(`foo`) < 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") < 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") < 1', self.pg.sql(func))

    def test_sum__lte(self):
        func = fn.sum(Name("foo")) <= 1
        self.assertEqual('SUM(`foo`) <= 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") <= 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") <= 1', self.pg.sql(func))

    def test_sum__eq(self):
        func = fn.sum(Name("foo")) == 1
        self.assertEqual('SUM(`foo`) = 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") = 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") = 1', self.pg.sql(func))

    def test_sum__not_eq(self):
        func = fn.sum(Name("foo")) != 1
        self.assertEqual('SUM(`foo`) <> 1', self.mysql.sql(func))
        self.assertEqual('SUM("foo") <> 1', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") <> 1', self.pg.sql(func))

    def test_sum__and(self):
        func = (fn.sum(Name("foo")) != 1) & (fn.sum(Name("foo")) < 100)
        self.assertEqual('SUM(`foo`) <> 1 AND SUM(`foo`) < 100', self.mysql.sql(func))
        self.assertEqual('SUM("foo") <> 1 AND SUM("foo") < 100', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") <> 1 AND SUM("foo") < 100', self.pg.sql(func))

    def test_sum__or(self):
        func = (fn.sum(Name("foo")) != 1) | (fn.sum(Name("foo")) < 100)
        self.assertEqual('SUM(`foo`) <> 1 OR SUM(`foo`) < 100', self.mysql.sql(func))
        self.assertEqual('SUM("foo") <> 1 OR SUM("foo") < 100', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") <> 1 OR SUM("foo") < 100', self.pg.sql(func))

    def test_sum__and_or_nesting(self):
        func = (fn.sum(Name("foo")) != 1) & ((fn.sum(Name("foo")) < 100) | (fn.sum(Name("bar")) > 20))
        self.assertEqual('SUM(`foo`) <> 1 AND (SUM(`foo`) < 100 OR SUM(`bar`) > 20)', self.mysql.sql(func))
        self.assertEqual('SUM("foo") <> 1 AND (SUM("foo") < 100 OR SUM("bar") > 20)', self.sqlite.sql(func))
        self.assertEqual('SUM("foo") <> 1 AND (SUM("foo") < 100 OR SUM("bar") > 20)', self.pg.sql(func))


if __name__ == '__main__':
    unittest.main()
