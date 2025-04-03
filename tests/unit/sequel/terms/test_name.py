import pytest

from sweet.sequel.terms import literal
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name


def test_name__equals_star(visitors):
    n1, n2, n3 = Name("*"), "*", literal.STAR
    assert n1 == n2
    assert n1 == n3


def test_value(visitors):
    assert Name("users").name == "users"
    assert Name("name").name == "name"
    assert Name("users.name").name == "users.name"


def test_sql_name_with_schema(visitors):
    age = Name('age', schema_name="users")
    assert visitors.mysql.sql(age) == '`users`.`age`'
    assert visitors.sqlite.sql(age) == '"users"."age"'
    assert visitors.pg.sql(age) == '"users"."age"'


def test_sql_of_name(visitors):
    n = Name("name")
    assert visitors.mysql.sql(n) == '`name`'
    assert visitors.sqlite.sql(n) == '"name"'
    assert visitors.pg.sql(n) == '"name"'

    n = Name("users.name")
    assert visitors.mysql.sql(n) == '`users`.`name`'
    assert visitors.sqlite.sql(n) == '"users"."name"'
    assert visitors.pg.sql(n) == '"users"."name"'


def test_sql_of_name_alias(visitors):
    n = Name("id").as_("user_id")
    assert visitors.mysql.sql(n) == '`id` AS `user_id`'
    assert visitors.sqlite.sql(n) == '"id" AS "user_id"'
    assert visitors.pg.sql(n) == '"id" AS "user_id"'


def test_eq(visitors):
    assert Name('name').eq('jim') == Binary.parse(name="jim")
    n = Name("nickname", "users")
    assert Name('name', 'users').eq(n) == Binary.parse(users__name=n)


def test_not_eq(visitors):
    assert Name('name').not_eq('jim') == Binary.parse(name__not='jim')
    assert Name('name', 'users').not_eq('jim') == Binary.parse(users__name__not='jim')
    assert Name('name', 'users').not_eq(Name("nickname", "users")) == Binary.parse(users__name__not=Name("nickname", "users"))


def test_is_null(visitors):
    assert Name('name').eq(None) == Binary.parse(name=None)


def test_is_not_null(visitors):
    assert Name('name').not_eq(None) == Binary.parse(name__not=None)


def test_in(visitors):
    assert Name('name').eq(['jim', 'lucy', 'lily']) == Binary.parse(name=['jim', 'lucy', 'lily'])


def test_not_in(visitors):
    assert Name('name').not_eq(['jim', 'lucy', 'lily']) == Binary.parse(name__not=['jim', 'lucy', 'lily'])


def test_gt(visitors):
    assert Name('age').gt(10) == Binary.parse(age__gt=10)


def test_not_gt(visitors):
    assert Name('age').not_gt(10) == Binary.parse(age__lte=10)


def test_gte(visitors):
    assert Name('age').gte(10) == Binary.parse(age__gte=10)


def test_not_gte(visitors):
    assert Name('age').not_gte(10) == Binary.parse(age__lt=10)


def test_lt(visitors):
    assert Name('age').lt(10) == Binary.parse(age__lt=10)


def test_not_lt(visitors):
    assert Name('age').not_lt(10) == Binary.parse(age__gte=10)


def test_lte(visitors):
    assert Name('age').lte(10) == Binary.parse(age__lte=10)


def test_not_lte(visitors):
    assert Name('age').not_lte(10) == Binary.parse(age__gt=10)


def test_like(visitors):
    assert Name('name').like('%jim') == Binary.parse(name__like='%jim')


def test_not_like(visitors):
    assert Name('name').not_like('%jim') == Binary.parse(name__not_like='%jim')


def test_between(visitors):
    assert Name('age').between([10, 60]) == Binary.parse(age__bt=[10, 60])


def test_between_err(visitors):
    with pytest.raises(ValueError, match='The "BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Name('age').between([10, 60, 30])


def test_not_between(visitors):
    assert Name('age').not_between([10, 60]) == Binary.parse(age__not_bt=[10, 60])


def test_not_between_err(visitors):
    with pytest.raises(ValueError, match='The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Name('age').not_between([10, 60, 30])


def test_pair_with_regex_value(visitors):
    assert Name('username', 'users').regex("^[b]abc") == Binary.parse(users__username__regex="^[b]abc")


def test_pair_with_not_regex_value(visitors):
    assert Name('username', 'users').not_regex("^[b]abc") == Binary.parse(users__username__not_regex="^[b]abc")
