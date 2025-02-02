import unittest

from sweet.sequel.terms import literal
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
        self.assertTrue(n3 == n2)
        self.assertTrue(n3 != 4)

    def test_value(self):
        self.assertEqual("users", Name("users").value)
        self.assertEqual("name", Name("name").value)
        self.assertEqual("users.name", Name("users.name").value)

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


if __name__ == '__main__':
    unittest.main()
