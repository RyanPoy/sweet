
from sweet.sequel import Operator
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name


def test_omit_where(visitors):
    stmt = DeleteStatement(Name("users"))
    assert visitors.mysql.sql(stmt) == 'DELETE FROM `users`'
    assert visitors.sqlite.sql(stmt) == 'DELETE FROM "users"'
    assert visitors.pg.sql(stmt) == 'DELETE FROM "users"'


def test_where_field_equals(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar")
    assert visitors.mysql.sql(stmt) == 'DELETE FROM `users` WHERE `foo` = \'bar\''
    assert visitors.sqlite.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\''
    assert visitors.pg.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\''


def test_where_chaining_field_equals(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar").where(baz="xyz")
    assert visitors.mysql.sql(stmt) == 'DELETE FROM `users` WHERE `foo` = \'bar\' AND `baz` = \'xyz\''
    assert visitors.sqlite.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\''
    assert visitors.pg.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\''


def test_where_q(visitors):
    stmt = DeleteStatement(Name("users")).where(Binary.parse(foo="bar") & Binary.parse(baz="xyz"))
    assert visitors.mysql.sql(stmt) == """DELETE FROM `users` WHERE `foo` = 'bar' AND `baz` = 'xyz'"""
    assert visitors.sqlite.sql(stmt) == """DELETE FROM "users" WHERE "foo" = 'bar' AND "baz" = 'xyz'"""
    assert visitors.pg.sql(stmt) == """DELETE FROM "users" WHERE "foo" = 'bar' AND "baz" = 'xyz'"""


def test_where_chaining_binary(visitors):
    stmt = DeleteStatement(Name("users")).where(Binary(Name('foo'), Operator.EQ, "bar")).where(baz="xyz")
    assert visitors.mysql.sql(stmt) == "DELETE FROM `users` WHERE `foo` = 'bar' AND `baz` = 'xyz'"
    assert visitors.sqlite.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\''
    assert visitors.pg.sql(stmt) == 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\''


def test_where_complex_args(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar").where(Binary.parse(baz="xyz") | Binary.parse(abc=123)).where(age__lt=30)
    assert visitors.mysql.sql(stmt) == """DELETE FROM `users` WHERE `foo` = 'bar' AND (`baz` = 'xyz' OR `abc` = 123) AND `age` < 30"""
    assert visitors.sqlite.sql(stmt) == """DELETE FROM "users" WHERE "foo" = 'bar' AND ("baz" = 'xyz' OR "abc" = 123) AND "age" < 30"""
    assert visitors.pg.sql(stmt) == """DELETE FROM "users" WHERE "foo" = 'bar' AND ("baz" = 'xyz' OR "abc" = 123) AND "age" < 30"""

# def test_delete_returning(visitors):
#     stmt = DeleteStatement(Name("users").where(Name("users".foo == Name("users".bar).returning(Name("users".id)
#     assert 'DELETE FROM "users" WHERE "foo"="bar" RETURNING "id"', str(dm)
#
# def test_delete_returning_str(visitors):
#     dm = (
#         DeleteStatement(Name("users")
#         .where(Name("users".foo == Name("users".bar)
#         .returning("id")
#     )
#
#     assert 'DELETE FROM "users" WHERE "foo"="bar" RETURNING "id"', str(dm)
#
# def test_delete_returning_star(visitors):
#     dm = (
#         DeleteStatement(Name("users")
#         .where(Name("users".foo == Name("users".bar)
#         .returning(Star())
#     )
#
#     assert 'DELETE FROM "users" WHERE "foo"="bar" RETURNING *', str(dm)
#
# def test_delete_using(visitors):
#     table_trash = Table('trash')
#     dm = (
#         DeleteStatement(Name("users")
#         .using(table_trash)
#         .where(Name("users".id == table_trash.abc_id)
#     )
#
#     assert 'DELETE FROM "users" USING "trash" WHERE "abc"."id"="trash"."abc_id"', str(dm)
