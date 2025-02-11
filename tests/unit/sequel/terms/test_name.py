import unittest

from sweet.sequel.terms import literal
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name import Name
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestName(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_name__equals_star(self):
        n1 = Name("*")
        n2 = "*"
        n3 = literal.STAR
        self.assertTrue(n1 == n2)
        self.assertTrue(n1 == n3)

    def test_value(self):
        self.assertEqual("users", Name("users").name)
        self.assertEqual("name", Name("name").name)
        self.assertEqual("users.name", Name("users.name").name)

    def test_sql_name_with_schema(self):
        age = Name('age', "users")
        self.assertEqual('`users`.`age`', self.mysql.sql(age))
        self.assertEqual('"users"."age"', self.sqlite.sql(age))
        self.assertEqual('"users"."age"', self.pg.sql(age))

    def test_sql_of_name(self):
        n = Name("name")
        self.assertEqual('`name`', self.mysql.sql(n))
        self.assertEqual('"name"', self.sqlite.sql(n))
        self.assertEqual('"name"', self.pg.sql(n))

        n = Name("users.name")
        self.assertEqual('`users`.`name`', self.mysql.sql(n))
        self.assertEqual('"users"."name"', self.sqlite.sql(n))
        self.assertEqual('"users"."name"', self.pg.sql(n))

    def test_sql_of_name_alias(self):
        alias = Name("id").as_("user_id")
        self.assertEqual('`id` AS `user_id`', self.mysql.sql(alias))
        self.assertEqual('"id" AS "user_id"', self.sqlite.sql(alias))
        self.assertEqual('"id" AS "user_id"', self.pg.sql(alias))

    def test_eq(self):
        n = Name('name').eq('jim')
        self.assertEqual(Binary.parse(name="jim"), n)

        n = Name('name', 'users').eq(Name("nickname", "users"))
        self.assertEqual(Binary.parse(users__name=Name("nickname", "users")), n)

    def test_not_eq(self):
        n = Name('name').not_eq('jim')
        self.assertEqual(Binary.parse(name__not='jim'), n)

        n = Name('name', 'users').not_eq('jim')
        self.assertEqual(Binary.parse(users__name__not='jim'), n)

        n = Name('name', 'users').not_eq(Name("nickname", "users"))
        self.assertEqual(Binary.parse(users__name__not=Name("nickname", "users")), n)

    def test_is_null(self):
        n = Name('name').eq(None)
        self.assertEqual(Binary.parse(name=None), n)

    def test_is_not_null(self):
        n = Name('name').not_eq(None)
        self.assertEqual(Binary.parse(name__not=None), n)

    def test_in(self):
        n = Name('name').eq(['jim', 'lucy', 'lily'])
        self.assertEqual(Binary.parse(name=['jim', 'lucy', 'lily']), n)

    def test_not_in(self):
        n = Name('name').not_eq(['jim', 'lucy', 'lily'])
        self.assertEqual(Binary.parse(name__not=['jim', 'lucy', 'lily']), n)

    def test_gt(self):
        n = Name('age').gt(10)
        self.assertEqual(Binary.parse(age__gt=10), n)

    def test_not_gt(self):
        n = Name('age').not_gt(10)
        self.assertEqual(Binary.parse(age__lte=10), n)

    def test_gte(self):
        n = Name('age').gte(10)
        self.assertEqual(Binary.parse(age__gte=10), n)

    def test_not_gte(self):
        n = Name('age').not_gte(10)
        self.assertEqual(Binary.parse(age__lt=10), n)

    def test_lt(self):
        n = Name('age').lt(10)
        self.assertEqual(Binary.parse(age__lt=10), n)

    def test_not_lt(self):
        n = Name('age').not_lt(10)
        self.assertEqual(Binary.parse(age__gte=10), n)

    def test_lte(self):
        n = Name('age').lte(10)
        self.assertEqual(Binary.parse(age__lte=10), n)

    def test_not_lte(self):
        n = Name('age').not_lte(10)
        self.assertEqual(Binary.parse(age__gt=10), n)

    def test_like(self):
        n = Name('name').like('%jim')
        self.assertEqual(Binary.parse(name__like='%jim'), n)

    def test_not_like(self):
        n = Name('name').not_like('%jim')
        self.assertEqual(Binary.parse(name__not_like='%jim'), n)

    def test_between(self):
        n = Name('age').between([10, 60])
        self.assertEqual(Binary.parse(age__bt=[10, 60]), n)

    def test_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            n = Name('age').between([10, 60, 30])
        self.assertEqual('The "BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_not_between(self):
        n = Name('age').not_between([10, 60])
        self.assertEqual(Binary.parse(age__not_bt=[10, 60]), n)

    def test_not_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            n = Name('age').not_between([10, 60, 30])
        self.assertEqual('The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_pair_with_regex_value(self):
        n = Name('username', 'users').regex("^[b]abc")
        self.assertEqual(Binary.parse(users__username__regex="^[b]abc"), n)

    def test_pair_with_not_regex_value(self):
        n = Name('username', 'users').not_regex("^[b]abc")
        self.assertEqual(Binary.parse(users__username__not_regex="^[b]abc"), n)


if __name__ == '__main__':
    unittest.main()
