import unittest

from sweet.sequel.terms.name import ColumnName, Name, TableName
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestAlias(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_sql(self):
        alias = ColumnName("id").as_("user_id")
        self.assertEqual('`id` AS `user_id`', alias.sql(self.mysql))
        self.assertEqual('"id" AS "user_id"', alias.sql(self.sqlite))
        self.assertEqual('"id" AS "user_id"', alias.sql(self.pg))

        alias = Name("users").as_("customers")
        self.assertEqual('`users` AS `customers`', alias.sql(self.mysql))
        self.assertEqual('"users" AS "customers"', alias.sql(self.sqlite))
        self.assertEqual('"users" AS "customers"', alias.sql(self.pg))


if __name__ == '__main__':
    unittest.main()