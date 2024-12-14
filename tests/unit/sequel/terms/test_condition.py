import unittest

from sweet.sequel.terms.condition import Condition
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
            Condition(a=1, b=2)
        self.assertEqual("Only one parameter is allowed for construction.", str(ctx.exception))

    def test_eq(self):
        c = Condition(name='jim')
        self.assertEqual("\"name\" = 'jim'", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" = 'jim'", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" = 'jim'", str(self.pg.visit(c)))

    def test_not_eq(self):
        c = Condition(name__not='jim')
        self.assertEqual("\"name\" <> 'jim'", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" <> 'jim'", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" <> 'jim'", str(self.pg.visit(c)))

    def test_eq_with___(self):
        c = Condition(nick__name='jim')
        self.assertEqual("\"nick__name\" = 'jim'", str(self.mysql.visit(c)))
        self.assertEqual("\"nick__name\" = 'jim'", str(self.sqlite.visit(c)))
        self.assertEqual("\"nick__name\" = 'jim'", str(self.pg.visit(c)))

    def test_is_null(self):
        c = Condition(name=None)
        self.assertEqual("\"name\" IS NULL", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" IS NULL", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" IS NULL", str(self.pg.visit(c)))

    def test_is_not_null(self):
        c = Condition(name__not=None)
        self.assertEqual("\"name\" IS NOT NULL", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" IS NOT NULL", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" IS NOT NULL", str(self.pg.visit(c)))

    def test_in(self):
        c = Condition(name=['jim', 'lucy', 'lily'])
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", str(self.pg.visit(c)))

    def test_not_in(self):
        c = Condition(name__not=['jim', 'lucy', 'lily'])
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", str(self.pg.visit(c)))

    def test_like(self):
        c = Condition(name__like='%jim')
        self.assertEqual("\"name\" LIKE '%jim'", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" LIKE '%jim'", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" LIKE '%jim'", str(self.pg.visit(c)))

    def test_not_like(self):
        c = Condition(name__not_like='%jim')
        self.assertEqual("\"name\" NOT LIKE '%jim'", str(self.mysql.visit(c)))
        self.assertEqual("\"name\" NOT LIKE '%jim'", str(self.sqlite.visit(c)))
        self.assertEqual("\"name\" NOT LIKE '%jim'", str(self.pg.visit(c)))

    def test_between(self):
        c = Condition(age__bt=[10, 60])
        self.assertEqual("\"age\" BETWEEN 10 AND 60", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", str(self.pg.visit(c)))

    def test_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Condition(age__bt=[10, 60, 10])
        self.assertEqual('The bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_not_between(self):
        c = Condition(age__not_bt=[10, 60])
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", str(self.pg.visit(c)))

    def test_not_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Condition(age__not_bt=[10, 60, 10])
        self.assertEqual('The not_bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_gt(self):
        c = Condition(age__gt=10)
        self.assertEqual("\"age\" > 10", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" > 10", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" > 10", str(self.pg.visit(c)))

    def test_gte(self):
        c = Condition(age__gte=10)
        self.assertEqual("\"age\" >= 10", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" >= 10", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" >= 10", str(self.pg.visit(c)))

    def test_lt(self):
        c = Condition(age__lt=30)
        self.assertEqual("\"age\" < 30", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" < 30", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" < 30", str(self.pg.visit(c)))

    def test_lte(self):
        c = Condition(age__lte=30)
        self.assertEqual("\"age\" <= 30", str(self.mysql.visit(c)))
        self.assertEqual("\"age\" <= 30", str(self.sqlite.visit(c)))
        self.assertEqual("\"age\" <= 30", str(self.pg.visit(c)))


if __name__ == '__main__':
    unittest.main()

