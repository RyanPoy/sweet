import unittest

from sweet.sequel.terms.pair import Pair
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestCondition(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_error_init(self):
        with self.assertRaises(ValueError) as ctx:
            Pair(a=1, b=2)
        self.assertEqual("Only one parameter is allowed for construction.", str(ctx.exception))

    def test_eq(self):
        c = Pair(name='jim')
        self.assertEqual("`name` = 'jim'", c.sql(self.mysql))
        self.assertEqual("\"name\" = 'jim'", c.sql(self.sqlite))
        self.assertEqual("\"name\" = 'jim'", c.sql(self.pg))

    def test_not_eq(self):
        c = Pair(name__not='jim')
        self.assertEqual("`name` <> 'jim'", c.sql(self.mysql))
        self.assertEqual("\"name\" <> 'jim'", c.sql(self.sqlite))
        self.assertEqual("\"name\" <> 'jim'", c.sql(self.pg))

    def test_eq_with___(self):
        c = Pair(nick__name='jim')
        self.assertEqual("`nick`.`name` = 'jim'", c.sql(self.mysql))
        self.assertEqual('"nick"."name" = \'jim\'', c.sql(self.sqlite))
        self.assertEqual('"nick"."name" = \'jim\'', c.sql(self.pg))

    def test_is_null(self):
        c = Pair(name=None)
        self.assertEqual("`name` IS NULL", c.sql(self.mysql))
        self.assertEqual("\"name\" IS NULL", c.sql(self.sqlite))
        self.assertEqual("\"name\" IS NULL", c.sql(self.pg))

    def test_is_not_null(self):
        c = Pair(name__not=None)
        self.assertEqual("`name` IS NOT NULL", c.sql(self.mysql))
        self.assertEqual("\"name\" IS NOT NULL", c.sql(self.sqlite))
        self.assertEqual("\"name\" IS NOT NULL", c.sql(self.pg))

    def test_in(self):
        c = Pair(name=['jim', 'lucy', 'lily'])
        self.assertEqual("`name` IN ('jim', 'lucy', 'lily')", c.sql(self.mysql))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", c.sql(self.sqlite))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", c.sql(self.pg))

    def test_not_in(self):
        c = Pair(name__not=['jim', 'lucy', 'lily'])
        self.assertEqual("`name` NOT IN ('jim', 'lucy', 'lily')", c.sql(self.mysql))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", c.sql(self.sqlite))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", c.sql(self.pg))

    def test_like(self):
        c = Pair(name__like='%jim')
        self.assertEqual("`name` LIKE '%jim'", c.sql(self.mysql))
        self.assertEqual("\"name\" LIKE '%jim'", c.sql(self.sqlite))
        self.assertEqual("\"name\" LIKE '%jim'", c.sql(self.pg))

    def test_not_like(self):
        c = Pair(name__not_like='%jim')
        self.assertEqual("`name` NOT LIKE '%jim'", c.sql(self.mysql))
        self.assertEqual("\"name\" NOT LIKE '%jim'", c.sql(self.sqlite))
        self.assertEqual("\"name\" NOT LIKE '%jim'", c.sql(self.pg))

    def test_between(self):
        c = Pair(age__bt=[10, 60])
        self.assertEqual("`age` BETWEEN 10 AND 60", c.sql(self.mysql))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", c.sql(self.sqlite))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", c.sql(self.pg))

    def test_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Pair(age__bt=[10, 60, 10])
        self.assertEqual('The bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_not_between(self):
        c = Pair(age__not_bt=[10, 60])
        self.assertEqual("`age` NOT BETWEEN 10 AND 60", c.sql(self.mysql))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", c.sql(self.sqlite))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", c.sql(self.pg))

    def test_not_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Pair(age__not_bt=[10, 60, 10])
        self.assertEqual('The not_bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_gt(self):
        c = Pair(age__gt=10)
        self.assertEqual("`age` > 10", c.sql(self.mysql))
        self.assertEqual("\"age\" > 10", c.sql(self.sqlite))
        self.assertEqual("\"age\" > 10", c.sql(self.pg))

    def test_gte(self):
        c = Pair(age__gte=10)
        self.assertEqual("`age` >= 10", c.sql(self.mysql))
        self.assertEqual("\"age\" >= 10", c.sql(self.sqlite))
        self.assertEqual("\"age\" >= 10", c.sql(self.pg))

    def test_lt(self):
        c = Pair(age__lt=30)
        self.assertEqual("`age` < 30", c.sql(self.mysql))
        self.assertEqual("\"age\" < 30", c.sql(self.sqlite))
        self.assertEqual("\"age\" < 30", c.sql(self.pg))

    def test_lte(self):
        c = Pair(age__lte=30)
        self.assertEqual("`age` <= 30", c.sql(self.mysql))
        self.assertEqual("\"age\" <= 30", c.sql(self.sqlite))
        self.assertEqual("\"age\" <= 30", c.sql(self.pg))


if __name__ == '__main__':
    unittest.main()

