
from sweet.sequel import Operator
from sweet.sequel.statements.delete_statement import DeleteStatement
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name


def test_omit_where(visitors):
    stmt = DeleteStatement(Name("users"))
    assert 'DELETE FROM `users`' == visitors.mysql.sql(stmt)
    assert 'DELETE FROM "users"' == visitors.sqlite.sql(stmt)
    assert 'DELETE FROM "users"' == visitors.pg.sql(stmt)


def test_where_field_equals(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar")
    assert 'DELETE FROM `users` WHERE `foo` = \'bar\'' == visitors.mysql.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\'' == visitors.sqlite.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\'' == visitors.pg.sql(stmt)


def test_where_chaining_field_equals(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar").where(baz="xyz")
    assert 'DELETE FROM `users` WHERE `foo` = \'bar\' AND `baz` = \'xyz\'' == visitors.mysql.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'' == visitors.sqlite.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'' == visitors.pg.sql(stmt)


def test_where_q(visitors):
    stmt = DeleteStatement(Name("users")).where(Binary.parse(foo="bar") & Binary.parse(baz="xyz"))
    assert """DELETE FROM `users` WHERE `foo` = 'bar' AND `baz` = 'xyz'""" == visitors.mysql.sql(stmt)
    assert """DELETE FROM "users" WHERE "foo" = 'bar' AND "baz" = 'xyz'""" == visitors.sqlite.sql(stmt)
    assert """DELETE FROM "users" WHERE "foo" = 'bar' AND "baz" = 'xyz'""" == visitors.pg.sql(stmt)


def test_where_chaining_binary(visitors):
    stmt = DeleteStatement(Name("users")).where(Binary(Name('foo'), Operator.EQ, "bar")).where(baz="xyz")
    assert "DELETE FROM `users` WHERE `foo` = 'bar' AND `baz` = 'xyz'" == visitors.mysql.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'' == visitors.sqlite.sql(stmt)
    assert 'DELETE FROM "users" WHERE "foo" = \'bar\' AND "baz" = \'xyz\'' == visitors.pg.sql(stmt)


def test_where_complex_args(visitors):
    stmt = DeleteStatement(Name("users")).where(foo="bar").where(Binary.parse(baz="xyz") | Binary.parse(abc=123)).where(age__lt=30)
    assert """DELETE FROM `users` WHERE `foo` = 'bar' AND (`baz` = 'xyz' OR `abc` = 123) AND `age` < 30""" == visitors.mysql.sql(stmt)
    assert """DELETE FROM "users" WHERE "foo" = 'bar' AND ("baz" = 'xyz' OR "abc" = 123) AND "age" < 30""" == visitors.sqlite.sql(stmt)
    assert """DELETE FROM "users" WHERE "foo" = 'bar' AND ("baz" = 'xyz' OR "abc" = 123) AND "age" < 30""" == visitors.pg.sql(stmt)

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
