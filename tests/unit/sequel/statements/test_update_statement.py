import unittest

from sweet.sequel.schema.table import Table
from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms.name import TableName
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestUpdateStatement(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()
        self.table_users = TableName("users")

    def test_empty_query(self):
        um = UpdateStatement().update(self.table_users)
        self.assertEqual("", um.sql(self.mysql))
        self.assertEqual("", um.sql(self.sqlite))
        self.assertEqual("", um.sql(self.pg))

    def test_omit_where(self):
        um = UpdateStatement().update(self.table_users).set(foo="bar")
        self.assertEqual('UPDATE "users" SET "foo" = \'bar\'', um.sql(self.mysql))
        self.assertEqual('UPDATE "users" SET "foo" = \'bar\'', um.sql(self.sqlite))
        self.assertEqual('UPDATE "users" SET "foo" = \'bar\'', um.sql(self.pg))

    def test_single_quote_escape_in_set(self):
        um = UpdateStatement().update(self.table_users).set(foo="bar'foo")
        self.assertEqual("UPDATE \"users\" SET \"foo\" = 'bar''foo'", um.sql(self.mysql))
        self.assertEqual("UPDATE \"users\" SET \"foo\" = 'bar''foo'", um.sql(self.sqlite))
        self.assertEqual("UPDATE \"users\" SET \"foo\" = 'bar''foo'", um.sql(self.pg))

    def test_update__table_schema(self):
        um = UpdateStatement().update(TableName("schema1.users")).set(foo=1).where(foo=0)

        self.assertEqual('UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0', um.sql(self.mysql))
        self.assertEqual('UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0', um.sql(self.sqlite))
        self.assertEqual('UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0', um.sql(self.pg))

    def test_update_with_none(self):
        um = UpdateStatement().update(self.table_users).set(foo=None)
        self.assertEqual('UPDATE "users" SET "foo" = NULL', um.sql(self.mysql))
        self.assertEqual('UPDATE "users" SET "foo" = NULL', um.sql(self.sqlite))
        self.assertEqual('UPDATE "users" SET "foo" = NULL', um.sql(self.pg))

    # def test_update_with_join(self):
    #     table_def = Table("def")
    #     um = UpdateStatement().update(self.table_users).set(foo=None).join(table_def).on(abc_id=self.table_users.id).set(lname=table_def.lname)
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', um.sql(self.mysql))
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', um.sql(self.sqlite))
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', um.sql(self.pg))

    # def test_update_with_limit(self):
    #     q = Query.update(self.table_users).set(self.table_users.lname, "test").limit(1)
    #     self.assertEqual('UPDATE "users" SET "lname"=\'test\' LIMIT 1', str(q))

    # def test_update_from(self):
    #     from_table = Table('from_table')
    #     q = UpdateStatement().update(self.table_users).set(lname="long_name").from_(from_table)
    #     self.assertEqual('UPDATE "users" SET "lname"="from_table"."long_name" FROM "from_table"', str(q))
#
#     def test_update_from_with_where(self):
#         from_table = Table('from_table')
#
#         q = (
#             Query.update(self.table_users)
#             .set(self.table_users.lname, from_table.long_name)
#             .from_(from_table)
#             .where(self.table_users.fname.eq(from_table.full_name))
#         )
#         self.assertEqual(
#             'UPDATE "users" SET "lname"="from_table"."long_name" FROM "from_table" '
#             'WHERE "users"."fname"="from_table"."full_name"',
#             str(q),
#         )
#
#     def test_update_with_statement(self):
#         table_efg = Table('efg')
#
#         sub_query = Query.from_(table_efg).select("fizz")
#         an_alias = AliasedQuery("an_alias")
#
#         q = (
#             Query.with_(sub_query, "an_alias")
#             .update(self.table_users)
#             .from_(an_alias)
#             .set(self.table_users.lname, an_alias.long_name)
#             .where(self.table_users.comp.eq(an_alias.alias_comp))
#         )
#         self.assertEqual(
#             'WITH an_alias AS (SELECT "fizz" FROM "efg") '
#             'UPDATE "users" SET "lname"="an_alias"."long_name" FROM an_alias '
#             'WHERE "users"."comp"="an_alias"."alias_comp"',
#             str(q),
#         )
#
#     def test_for_portion(self):
#         with self.subTest("with system time"):
#             q = Query.update(self.table_users.for_portion(SYSTEM_TIME.from_to('2020-01-01', '2020-02-01'))).set(
#                 "foo", "bar"
#             )
#
#             self.assertEqual(
#                 'UPDATE "users" FOR PORTION OF SYSTEM_TIME FROM \'2020-01-01\' TO \'2020-02-01\' SET "foo"=\'bar\'',
#                 str(q),
#             )
#
#         with self.subTest("with column"):
#             q = Query.update(
#                 self.table_users.for_portion(self.table_users.valid_period.from_to('2020-01-01', '2020-02-01'))
#             ).set("foo", "bar")
#
#             self.assertEqual(
#                 'UPDATE "users" FOR PORTION OF "valid_period" FROM \'2020-01-01\' TO \'2020-02-01\' SET "foo"=\'bar\'',
#                 str(q),
#             )
#
#
# class PostgresUpdateTests(unittest.TestCase):
#     table_users = Table("users")
#
#     def test_update_returning_str(self):
#         q = PostgreSQLQuery.update(self.table_users).where(self.table_users.foo == 0).set("foo", "bar").returning("id")
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING "users"."id"', str(q))
#
#     def test_update_returning(self):
#         q = (
#             PostgreSQLQuery.update(self.table_users)
#             .where(self.table_users.foo == 0)
#             .set("foo", "bar")
#             .returning(self.table_users.id)
#         )
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING "users"."id"', str(q))
#
#     def test_update_returning_from_different_tables(self):
#         table_bcd = Table('bcd')
#
#         q = (
#             PostgreSQLQuery.update(self.table_users)
#             .from_(table_bcd)
#             .set(self.table_users.lname, table_bcd.long_name)
#             .returning(self.table_users.id, table_bcd.fname)
#         )
#         self.assertEqual(
#             'UPDATE "users" SET "lname"="bcd"."long_name" FROM "bcd" RETURNING "users"."id","bcd"."fname"', str(q)
#         )
#
#     def test_update_returning_star(self):
#         q = PostgreSQLQuery.update(self.table_users).where(self.table_users.foo == 0).set("foo", "bar").returning(Star())
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING *', str(q))
#
#
# class SQLLiteUpdateTests(unittest.TestCase):
#     table_users = Table("users")
#
#     def test_update_with_bool(self):
#         q = SQLLiteQuery.update(self.table_users).set(self.table_users.foo, True)
#
#         self.assertEqual('UPDATE "users" SET "foo"=1', str(q))