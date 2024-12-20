import unittest

from sweet.sequel.terms.name import AliasName, ColumnName, TableName
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestName(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_value(self):
        self.assertEqual("users", TableName("users").value)
        self.assertEqual("name", ColumnName("name").value)
        self.assertEqual("users.name", ColumnName("users.name").value)
        self.assertEqual("users", AliasName("users").value)

    def test_sql_of_table_name(self):
        n = TableName("users")
        self.assertEqual('"users"', str(n.sql(self.mysql)))
        self.assertEqual('"users"', str(n.sql(self.sqlite)))
        self.assertEqual('"users"', str(n.sql(self.pg)))

    def test_sql_of_column_name(self):
        n = ColumnName("name")
        self.assertEqual('"name"', str(n.sql(self.mysql)))
        self.assertEqual('"name"', str(n.sql(self.sqlite)))
        self.assertEqual('"name"', str(n.sql(self.pg)))

        n = ColumnName("users.name")
        self.assertEqual('"users"."name"', str(n.sql(self.mysql)))
        self.assertEqual('"users"."name"', str(n.sql(self.sqlite)))
        self.assertEqual('"users"."name"', str(n.sql(self.pg)))


if __name__ == '__main__':
    unittest.main()
