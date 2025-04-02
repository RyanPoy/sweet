from sweet.sequel.statements.update_statement import UpdateStatement
from sweet.sequel.terms.name_fn import Name


def test_empty(visitors):
    stmt = UpdateStatement(Name("users"))
    assert "" == visitors.mysql.sql(stmt)
    assert "" == visitors.sqlite.sql(stmt)
    assert "" == visitors.pg.sql(stmt)


def test_update_to_other_column(visitors):
    stmt = UpdateStatement(Name("users")).set(foo=Name("bar"))
    assert 'UPDATE `users` SET `foo` = `bar`' == visitors.mysql.sql(stmt)
    assert 'UPDATE "users" SET "foo" = "bar"' == visitors.sqlite.sql(stmt)
    assert 'UPDATE "users" SET "foo" = "bar"' == visitors.pg.sql(stmt)


def test_omit_where(visitors):
    stmt = UpdateStatement(Name("users")).set(foo="bar")
    assert 'UPDATE `users` SET `foo` = \'bar\'' == visitors.mysql.sql(stmt)
    assert 'UPDATE "users" SET "foo" = \'bar\'' == visitors.sqlite.sql(stmt)
    assert 'UPDATE "users" SET "foo" = \'bar\'' == visitors.pg.sql(stmt)


def test_single_quote_escape_in_set(visitors):
    stmt = UpdateStatement(Name("users")).set(foo="bar'foo")
    assert "UPDATE `users` SET `foo` = 'bar''foo'" == visitors.mysql.sql(stmt)
    assert "UPDATE \"users\" SET \"foo\" = 'bar''foo'" == visitors.sqlite.sql(stmt)
    assert "UPDATE \"users\" SET \"foo\" = 'bar''foo'" == visitors.pg.sql(stmt)


def test_update__table_schema(visitors):
    stmt = UpdateStatement(Name("schema1.users")).set(foo=1).where(foo=0)

    assert 'UPDATE `schema1`.`users` SET `foo` = 1 WHERE `foo` = 0' == visitors.mysql.sql(stmt)
    assert 'UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0' == visitors.sqlite.sql(stmt)
    assert 'UPDATE "schema1"."users" SET "foo" = 1 WHERE "foo" = 0' == visitors.pg.sql(stmt)


def test_update_with_none(visitors):
    stmt = UpdateStatement(Name("users")).set(foo=None)
    assert 'UPDATE `users` SET `foo` = NULL' == visitors.mysql.sql(stmt)
    assert 'UPDATE "users" SET "foo" = NULL' == visitors.sqlite.sql(stmt)
    assert 'UPDATE "users" SET "foo" = NULL' == visitors.pg.sql(stmt)

    # def test_update_with_join(visitors):
    #     table_def = Table("def")
    #     stmt = UpdateStatement(Name("users")).set(foo=None).join(table_def).on(abc_id=Name("users").id).set(lname=table_def.lname)
    # assert 'UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"' == visitors.mysql.sql(stmt)
    # assert 'UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"' == visitors.sqlite.sql(stmt)
    # assert 'UPDATE "users" JOIN "def" ON "def"."abc_id" = "users"."id" SET "lname" = "def"."lname"' == visitors.pg.sql(stmt)

# def test_update_with_limit(visitors):
#     q = Query.update(Name("users")).set(Name("users").lname, "test").limit(1)
#     self.assertEqual('UPDATE "users" SET "lname"=\'test\' LIMIT 1', str(q))

# def test_update_from(visitors):
#     from_table = Table('from_table')
#     q = UpdateStatement(Name("users")).set(lname="long_name").from_(from_table)
#     self.assertEqual('UPDATE "users" SET "lname"="from_table"."long_name" FROM "from_table"', str(q))
#
#     def test_update_from_with_where(visitors):
#         from_table = Table('from_table')
#
#         q = (
#             Query.update(Name("users"))
#             .set(Name("users").lname, from_table.long_name)
#             .from_(from_table)
#             .where(Name("users").fname.eq(from_table.full_name))
#         )
#         self.assertEqual(
#             'UPDATE "users" SET "lname"="from_table"."long_name" FROM "from_table" '
#             'WHERE "users"."fname"="from_table"."full_name"',
#             str(q),
#         )
#
#     def test_update_with_statement(visitors):
#         table_efg = Table('efg')
#
#         sub_query = Query.from_(table_efg).select("fizz")
#         an_alias = AliasedQuery("an_alias")
#
#         q = (
#             Query.with_(sub_query, "an_alias")
#             .update(Name("users"))
#             .from_(an_alias)
#             .set(Name("users").lname, an_alias.long_name)
#             .where(Name("users").comp.eq(an_alias.alias_comp))
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
#     def test_update_returning_str(visitors):
#         q = PostgreSQLQuery.update(Name("users")).where(Name("users").foo == 0).set("foo", "bar").returning("id")
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING "users"."id"', str(q))
#
#     def test_update_returning(visitors):
#         q = (
#             PostgreSQLQuery.update(Name("users"))
#             .where(Name("users").foo == 0)
#             .set("foo", "bar")
#             .returning(Name("users").id)
#         )
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING "users"."id"', str(q))
#
#     def test_update_returning_from_different_tables(visitors):
#         table_bcd = Table('bcd')
#
#         q = (
#             PostgreSQLQuery.update(Name("users"))
#             .from_(table_bcd)
#             .set(Name("users").lname, table_bcd.long_name)
#             .returning(Name("users").id, table_bcd.fname)
#         )
#         self.assertEqual(
#             'UPDATE "users" SET "lname"="bcd"."long_name" FROM "bcd" RETURNING "users"."id","bcd"."fname"', str(q)
#         )
#
#     def test_update_returning_star(visitors):
#         q = PostgreSQLQuery.update(Name("users")).where(Name("users").foo == 0).set("foo", "bar").returning(Star())
#
#         self.assertEqual('UPDATE "users" SET "foo"=\'bar\' WHERE "foo"=0 RETURNING *', str(q))
#
#
# class SQLLiteUpdateTests(unittest.TestCase):
#     table_users = Table("users")
#
#     def test_update_with_bool(visitors):
#         q = SQLLiteQuery.update(Name("users")).set(Name("users").foo, True)
#
#         self.assertEqual('UPDATE "users" SET "foo"=1', str(q))
