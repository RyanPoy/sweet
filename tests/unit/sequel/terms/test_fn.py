import unittest

from sweet.sequel.terms import fn
from sweet.sequel.terms.name import ColumnName
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestFn(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_count__star(self):
        func = fn.count("*")
        self.assertEqual('COUNT(*)', func.sql(self.mysql))
        self.assertEqual('COUNT(*)', func.sql(self.sqlite))
        self.assertEqual('COUNT(*)', func.sql(self.pg))

    def test_count__str(self):
        func = fn.count("bar")
        self.assertEqual('COUNT(`bar`)', func.sql(self.mysql))
        self.assertEqual('COUNT("bar")', func.sql(self.sqlite))
        self.assertEqual('COUNT("bar")', func.sql(self.pg))

    def test_count__columnName(self):
        func = fn.count(ColumnName("bar"))
        self.assertEqual('COUNT(`bar`)', func.sql(self.mysql))
        self.assertEqual('COUNT("bar")', func.sql(self.sqlite))
        self.assertEqual('COUNT("bar")', func.sql(self.pg))

    def test_count_distinct(self):
        func = fn.count("foo").distinct()
        self.assertEqual('COUNT(DISTINCT `foo`)', func.sql(self.mysql))
        self.assertEqual('COUNT(DISTINCT "foo")', func.sql(self.sqlite))
        self.assertEqual('COUNT(DISTINCT "foo")', func.sql(self.pg))

    def test_sum_distinct(self):
        func = fn.sum("foo").distinct()
        self.assertEqual('SUM(DISTINCT `foo`)', func.sql(self.mysql))
        self.assertEqual('SUM(DISTINCT "foo")', func.sql(self.sqlite))
        self.assertEqual('SUM(DISTINCT "foo")', func.sql(self.pg))

    def test_sum__gt(self):
        func = fn.sum("foo") > 1
        self.assertEqual('SUM(`foo`) > 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") > 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") > 1', func.sql(self.pg))

    def test_sum__gte(self):
        func = fn.sum("foo") >= 1
        self.assertEqual('SUM(`foo`) >= 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") >= 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") >= 1', func.sql(self.pg))

    def test_sum__lt(self):
        func = fn.sum("foo") < 1
        self.assertEqual('SUM(`foo`) < 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") < 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") < 1', func.sql(self.pg))

    def test_sum__lte(self):
        func = fn.sum("foo") <= 1
        self.assertEqual('SUM(`foo`) <= 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") <= 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") <= 1', func.sql(self.pg))

    def test_sum__eq(self):
        func = fn.sum("foo") == 1
        self.assertEqual('SUM(`foo`) = 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") = 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") = 1', func.sql(self.pg))

    def test_sum__not_eq(self):
        func = fn.sum("foo") != 1
        self.assertEqual('SUM(`foo`) <> 1', func.sql(self.mysql))
        self.assertEqual('SUM("foo") <> 1', func.sql(self.sqlite))
        self.assertEqual('SUM("foo") <> 1', func.sql(self.pg))


if __name__ == '__main__':
    unittest.main()
