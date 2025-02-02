import unittest

from sweet.sequel.terms import literal
from sweet.sequel.terms.name import ColumnName, TableName
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
        self.assertEqual("users", TableName("users").value)
        self.assertEqual("name", ColumnName("name").value)
        self.assertEqual("users.name", ColumnName("users.name").value)

    def test_sql_for_column_name_from_table_name(self):
        t = TableName("users")
        self.assertEqual('`users`.`age`', t.age.sql(self.mysql))
        self.assertEqual('"users"."age"', t.age.sql(self.sqlite))
        self.assertEqual('"users"."age"', t.age.sql(self.pg))

    def test_sql_of_table_name(self):
        n = TableName("users")
        self.assertEqual('`users`', n.sql(self.mysql))
        self.assertEqual('"users"', n.sql(self.sqlite))
        self.assertEqual('"users"', n.sql(self.pg))

    def test_sql_of_column_name(self):
        n = ColumnName("name")
        self.assertEqual('`name`', n.sql(self.mysql))
        self.assertEqual('"name"', n.sql(self.sqlite))
        self.assertEqual('"name"', n.sql(self.pg))

        n = ColumnName("users.name")
        self.assertEqual('`users`.`name`', n.sql(self.mysql))
        self.assertEqual('"users"."name"', n.sql(self.sqlite))
        self.assertEqual('"users"."name"', n.sql(self.pg))


if __name__ == '__main__':
    unittest.main()
