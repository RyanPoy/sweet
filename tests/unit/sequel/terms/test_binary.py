import unittest

from sweet.sequel.terms.name import Name
from sweet.sequel.terms.binary import Binary
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestBinary(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    # def test_error_init(self):
    #     with self.assertRaises(ValueError) as ctx:
    #         Binary(a=1, b=2)
    #     self.assertEqual("Only one parameter is allowed for construction.", str(ctx.exception))
    #
    # def test_eq(self):
    #     p = Binary(name='jim')
    #     self.assertEqual("`name` = 'jim'", self.mysql.sql(p))
    #     self.assertEqual("\"name\" = 'jim'", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" = 'jim'", self.pg.sql(p))
    #
    # def test_not_eq(self):
    #     p = Binary(name__not='jim')
    #     self.assertEqual("`name` <> 'jim'", self.mysql.sql(p))
    #     self.assertEqual("\"name\" <> 'jim'", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" <> 'jim'", self.pg.sql(p))
    #
    # def test_eq_with___(self):
    #     p = Binary(nick__name='jim')
    #     self.assertEqual("`nick`.`name` = 'jim'", self.mysql.sql(p))
    #     self.assertEqual('"nick"."name" = \'jim\'', self.sqlite.sql(p))
    #     self.assertEqual('"nick"."name" = \'jim\'', self.pg.sql(p))
    #
    # def test_is_null(self):
    #     p = Binary(name=None)
    #     self.assertEqual("`name` IS NULL", self.mysql.sql(p))
    #     self.assertEqual("\"name\" IS NULL", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" IS NULL", self.pg.sql(p))
    #
    # def test_is_not_null(self):
    #     p = Binary(name__not=None)
    #     self.assertEqual("`name` IS NOT NULL", self.mysql.sql(p))
    #     self.assertEqual("\"name\" IS NOT NULL", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" IS NOT NULL", self.pg.sql(p))
    #
    # def test_in(self):
    #     p = Binary(name=['jim', 'lucy', 'lily'])
    #     self.assertEqual("`name` IN ('jim', 'lucy', 'lily')", self.mysql.sql(p))
    #     self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", self.pg.sql(p))
    #
    # def test_not_in(self):
    #     p = Binary(name__not=['jim', 'lucy', 'lily'])
    #     self.assertEqual("`name` NOT IN ('jim', 'lucy', 'lily')", self.mysql.sql(p))
    #     self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", self.pg.sql(p))
    #
    # def test_like(self):
    #     p = Binary(name__like='%jim')
    #     self.assertEqual("`name` LIKE '%jim'", self.mysql.sql(p))
    #     self.assertEqual("\"name\" LIKE '%jim'", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" LIKE '%jim'", self.pg.sql(p))
    #
    # def test_not_like(self):
    #     p = Binary(name__not_like='%jim')
    #     self.assertEqual("`name` NOT LIKE '%jim'", self.mysql.sql(p))
    #     self.assertEqual("\"name\" NOT LIKE '%jim'", self.sqlite.sql(p))
    #     self.assertEqual("\"name\" NOT LIKE '%jim'", self.pg.sql(p))
    #
    # def test_between(self):
    #     p = Binary(age__bt=[10, 60])
    #     self.assertEqual("`age` BETWEEN 10 AND 60", self.mysql.sql(p))
    #     self.assertEqual("\"age\" BETWEEN 10 AND 60", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" BETWEEN 10 AND 60", self.pg.sql(p))
    #
    # def test_between_err(self):
    #     with self.assertRaises(ValueError) as ctx:
    #         Binary(age__bt=[10, 60, 10])
    #     self.assertEqual('The bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))
    #
    # def test_not_between(self):
    #     p = Binary(age__not_bt=[10, 60])
    #     self.assertEqual("`age` NOT BETWEEN 10 AND 60", self.mysql.sql(p))
    #     self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", self.pg.sql(p))
    #
    # def test_not_between_err(self):
    #     with self.assertRaises(ValueError) as ctx:
    #         Binary(age__not_bt=[10, 60, 10])
    #     self.assertEqual('The not_bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))
    #
    # def test_gt(self):
    #     p = Binary(age__gt=10)
    #     self.assertEqual("`age` > 10", self.mysql.sql(p))
    #     self.assertEqual("\"age\" > 10", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" > 10", self.pg.sql(p))
    #
    # def test_gte(self):
    #     p = Binary(age__gte=10)
    #     self.assertEqual("`age` >= 10", self.mysql.sql(p))
    #     self.assertEqual("\"age\" >= 10", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" >= 10", self.pg.sql(p))
    #
    # def test_lt(self):
    #     p = Binary(age__lt=30)
    #     self.assertEqual("`age` < 30", self.mysql.sql(p))
    #     self.assertEqual("\"age\" < 30", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" < 30", self.pg.sql(p))
    #
    # def test_lte(self):
    #     p = Binary(age__lte=30)
    #     self.assertEqual("`age` <= 30", self.mysql.sql(p))
    #     self.assertEqual("\"age\" <= 30", self.sqlite.sql(p))
    #     self.assertEqual("\"age\" <= 30", self.pg.sql(p))
    #
    # def test_pair_with_Name_value(self):
    #     p = Binary(users__username=Name("nickname", "users"))
    #     self.assertEqual('`users`.`username` = `users`.`nickname`', self.mysql.sql(p))
    #     self.assertEqual('"users"."username" = "users"."nickname"', self.sqlite.sql(p))
    #     self.assertEqual('"users"."username" = "users"."nickname"', self.pg.sql(p))
    #
    # def test_pair_with_regex_value(self):
    #     p = Binary(users__username__regex="^[b]abc")
    #     self.assertEqual("`users`.`username` REGEX '^[b]abc'", self.mysql.sql(p))
    #     self.assertEqual("\"users\".\"username\" REGEX '^[b]abc'", self.sqlite.sql(p))
    #     self.assertEqual("\"users\".\"username\" REGEX '^[b]abc'", self.pg.sql(p))


if __name__ == '__main__':
    unittest.main()
