import unittest

from sweet.sequel.schema.table import Table
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class DeleteStatementTest(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()
        self.table_users = Table("users")

    def test_omit_where(self):
        dm = DeleteStatement().from_(self.table_users)
        self.assertEqual('DELETE FROM "users"', dm.sql(self.mysql))
        self.assertEqual('DELETE FROM "users"', dm.sql(self.sqlite))
        self.assertEqual('DELETE FROM "users"', dm.sql(self.sqlite))

    def test_where_field_equals(self):
        q1 = Query.from_(self.table_abc).where(self.table_abc.foo == self.table_abc.bar)
        q2 = Query.from_(self.table_abc).where(self.table_abc.foo.eq(self.table_abc.bar))

        self.assertEqual('DELETE FROM "abc" WHERE "foo"="bar"', str(q1))
        self.assertEqual('DELETE FROM "abc" WHERE "foo"="bar"', str(q2))

#     def test_for_portion(self):
#         with self.subTest("with system time"):
#             q = Query.from_(self.table_abc.for_portion(SYSTEM_TIME.from_to('2020-01-01', '2020-02-01')))
#
#             self.assertEqual(
#                 'DELETE FROM "abc" FOR PORTION OF SYSTEM_TIME FROM \'2020-01-01\' TO \'2020-02-01\'', str(q)
#             )
#
#         with self.subTest("with column"):
#             q = Query.from_(
#                 self.table_abc.for_portion(self.table_abc.valid_period.from_to('2020-01-01', '2020-02-01'))
#             )
#
#             self.assertEqual(
#                 'DELETE FROM "abc" FOR PORTION OF "valid_period" FROM \'2020-01-01\' TO \'2020-02-01\'', str(q)
#             )
#
#
# class PostgresDeleteTests(unittest.TestCase):
#     @classmethod
#     def setUpClass(cls) -> None:
#         super().setUpClass()
#         cls.table_abc = Table("abc")
#
#     def test_delete_returning(self):
#         q1 = (
#             PostgreSQLQuery.from_(self.table_abc)
#             .where(self.table_abc.foo == self.table_abc.bar)
#             .returning(self.table_abc.id)
#         )
#
#         self.assertEqual('DELETE FROM "abc" WHERE "foo"="bar" RETURNING "id"', str(q1))
#
#     def test_delete_returning_str(self):
#         q1 = (
#             PostgreSQLQuery.from_(self.table_abc)
#             .where(self.table_abc.foo == self.table_abc.bar)
#             .returning("id")
#         )
#
#         self.assertEqual('DELETE FROM "abc" WHERE "foo"="bar" RETURNING "id"', str(q1))
#
#     def test_delete_returning_star(self):
#         q1 = (
#             PostgreSQLQuery.from_(self.table_abc)
#             .where(self.table_abc.foo == self.table_abc.bar)
#             .returning(Star())
#         )
#
#         self.assertEqual('DELETE FROM "abc" WHERE "foo"="bar" RETURNING *', str(q1))
#
#     def test_delete_using(self):
#         table_trash = Table('trash')
#         q1 = (
#             PostgreSQLQuery.from_(self.table_abc)
#             .using(table_trash)
#             .where(self.table_abc.id == table_trash.abc_id)
#         )
#
#         self.assertEqual('DELETE FROM "abc" USING "trash" WHERE "abc"."id"="trash"."abc_id"', str(q1))