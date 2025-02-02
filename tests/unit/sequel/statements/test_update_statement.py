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
        stmt = UpdateStatement().update(self.table_users)
        self.assertEqual("", self.mysql.sql(stmt))
        self.assertEqual("", self.sqlite.sql(stmt))
        self.assertEqual("", self.pg.sql(stmt))

    def test_omit_where(self):
        stmt = UpdateStatement().update(self.table_users).set(foo="bar")
        self.assertEqual('UPDATE `users` SET `foo` = \'bar\'', self.mysql.sql(stmt))
        self.assertEqual('UPDATE "users" SET "foo" = \'bar\'', self.sqlite.sql(stmt))
        self.assertEqual('UPDATE "users" SET "foo" = \'bar\'', self.pg.sql(stmt))

    def test_single_quote_escape_in_set(self):
        stmt = UpdateStatement().update(self.table_users).set(foo="bar'foo")
        self.assertEqual("UPDATE `users` SET `foo` = 'bar''foo'", self.mysql.sql(stmt))
        self.assertEqual("UPDATE \"users\" SET \"foo\" = 'bar''foo'", self.sqlite.sql(stmt))
        self.assertEqual("UPDATE \"users\" SET \"foo\" = 'bar''foo'", self.pg.sql(stmt))

    def test_update__table_schema(self):
        stmt = UpdateStatement().update(TableName("schema1.users")).set(foo=1).where(foo=0)

        self.assertEqual('UPDATE `schema1`.`users` SET `foo` = 1 WHERE `foo` = 0', self.mysql.sql(stmt))
        self.assertEqual('UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0', self.sqlite.sql(stmt))
        self.assertEqual('UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0', self.pg.sql(stmt))

    def test_update_with_none(self):
        stmt = UpdateStatement().update(self.table_users).set(foo=None)
        self.assertEqual('UPDATE `users` SET `foo` = NULL', self.mysql.sql(stmt))
        self.assertEqual('UPDATE "users" SET "foo" = NULL', self.sqlite.sql(stmt))
        self.assertEqual('UPDATE "users" SET "foo" = NULL', self.pg.sql(stmt))

    # def test_update_with_join(self):
    #     table_def = Table("def")
    #     stmt = UpdateStatement().update(self.table_users).set(foo=None).join(table_def).on(abc_id=self.table_users.id).set(lname=table_def.lname)
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', self.mysql.sql(stmt))
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', self.sqlite.sql(stmt))
    #     self.assertEqual('UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"', self.pg.sql(stmt))

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