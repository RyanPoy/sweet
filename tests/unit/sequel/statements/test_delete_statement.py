import unittest

from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.terms.name import TableName
from sweet.sequel.terms.q import Q
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestDeleteStatement(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()
        self.table_users = TableName("users")

    def test_omit_where(self):
        stmt = DeleteStatement().from_(self.table_users)
        self.assertEqual('DELETE FROM "users"', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users"', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users"', stmt.sql(self.pg))

    def test_where_field_equals(self):
        stmt = DeleteStatement().from_(self.table_users).where(foo="bar")
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\'', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\'', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\'', stmt.sql(self.pg))

    def test_where_chaining_field_equals(self):
        stmt = DeleteStatement().from_(self.table_users).where(foo="bar").where(baz="xyz")
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.pg))

    def test_where_q(self):
        stmt = DeleteStatement().from_(self.table_users).where(Q(foo="bar") & Q(baz="xyz"))
        self.assertEqual('DELETE FROM "users" WHERE ("foo" = \'bar\' AND "baz" = \'xyz\')', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE ("foo" = \'bar\' AND "baz" = \'xyz\')', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE ("foo" = \'bar\' AND "baz" = \'xyz\')', stmt.sql(self.pg))

    def test_where_chaining_q(self):
        stmt = DeleteStatement().from_(self.table_users).where(Q(foo="bar")).where(Q(baz="xyz"))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'', stmt.sql(self.pg))

    def test_where_complex_args(self):
        stmt = DeleteStatement().from_(self.table_users).where(foo="bar").where(Q(baz="xyz") | Q(abc=123)).where(age__lt=30)
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND ("baz" = \'xyz\' OR "abc" = 123) AND "age" < 30', stmt.sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND ("baz" = \'xyz\' OR "abc" = 123) AND "age" < 30', stmt.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE "foo" = \'bar\' AND ("baz" = \'xyz\' OR "abc" = 123) AND "age" < 30', stmt.sql(self.pg))

    # def test_delete_returning(self):
    #     stmt = DeleteStatement().from_(self.table_users).where(self.table_users.foo == self.table_users.bar).returning(self.table_users.id)
    #     self.assertEqual('DELETE FROM "users" WHERE "foo"="bar" RETURNING "id"', str(dm))
    #
    # def test_delete_returning_str(self):
    #     dm = (
    #         DeleteStatement().from_(self.table_users)
    #         .where(self.table_users.foo == self.table_users.bar)
    #         .returning("id")
    #     )
    #
    #     self.assertEqual('DELETE FROM "users" WHERE "foo"="bar" RETURNING "id"', str(dm))
    #
    # def test_delete_returning_star(self):
    #     dm = (
    #         DeleteStatement().from_(self.table_users)
    #         .where(self.table_users.foo == self.table_users.bar)
    #         .returning(Star())
    #     )
    #
    #     self.assertEqual('DELETE FROM "users" WHERE "foo"="bar" RETURNING *', str(dm))
    #
    # def test_delete_using(self):
    #     table_trash = Table('trash')
    #     dm = (
    #         DeleteStatement().from_(self.table_users)
    #         .using(table_trash)
    #         .where(self.table_users.id == table_trash.abc_id)
    #     )
    #
    #     self.assertEqual('DELETE FROM "users" USING "trash" WHERE "abc"."id"="trash"."abc_id"', str(dm))


if __name__ == '__main__':
    unittest.main()
