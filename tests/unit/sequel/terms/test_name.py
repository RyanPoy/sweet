import unittest

from sweet.sequel.terms import literal
from sweet.sequel.terms.name import ColumnName, Name
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestName(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_columnname__equals_star(self):
        n1 = ColumnName("*")
        n2 = "*"
        n3 = literal.STAR
        self.assertTrue(n1 == n2)
        self.assertTrue(n1 == n3)
        self.assertTrue(n3 == n2)

    def test_value(self):
        self.assertEqual("users", Name("users").value)
        self.assertEqual("name", ColumnName("name").value)
        self.assertEqual("users.name", ColumnName("users.name").value)

    def test_sql_column_name_with_schema(self):
        age = Name('age', "users")
        self.assertEqual('`users`.`age`', self.mysql.sql(age))
        self.assertEqual('"users"."age"', self.sqlite.sql(age))
        self.assertEqual('"users"."age"', self.pg.sql(age))

    def test_sql_of_table_name(self):
        n = Name("users")
        self.assertEqual('`users`', self.mysql.sql(n))
        self.assertEqual('"users"', self.sqlite.sql(n))
        self.assertEqual('"users"', self.pg.sql(n))

    def test_sql_of_column_name(self):
        n = ColumnName("name")
        self.assertEqual('`name`', self.mysql.sql(n))
        self.assertEqual('"name"', self.sqlite.sql(n))
        self.assertEqual('"name"', self.pg.sql(n))

        n = ColumnName("users.name")
        self.assertEqual('`users`.`name`', self.mysql.sql(n))
        self.assertEqual('"users"."name"', self.sqlite.sql(n))
        self.assertEqual('"users"."name"', self.pg.sql(n))


if __name__ == '__main__':
    unittest.main()
