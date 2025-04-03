import pytest
from sweet.sequel import Operator
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.terms.binary import Binary


def test_error_init(visitors):
    with pytest.raises(ValueError, match="Only one parameter is allowed for construction."):
        Binary.parse(a=1, b=2)


def test_init_from_parse(visitors):
    assert Binary.parse(name='jim') == Binary(Name("name"), Operator.EQ, 'jim')
    assert Binary.parse(name__not='jim') == Binary(Name("name"), Operator.NOT_EQ, 'jim')
    assert Binary.parse(nick__name='jim') == Binary(Name("name", schema_name="nick"), Operator.EQ, 'jim')
    assert Binary.parse(name=None) == Binary(Name("name"), Operator.IS, None)
    assert Binary.parse(name__not=None) == Binary(Name("name"), Operator.IS_NOT, None)
    assert Binary.parse(name=['jim', 'lucy', 'lily']) == Binary(Name("name"), Operator.IN, ['jim', 'lucy', 'lily'])
    assert Binary.parse(name__not=['jim', 'lucy', 'lily']) == Binary(Name("name"), Operator.NOT_IN, ['jim', 'lucy', 'lily'])
    assert Binary.parse(name__like='%jim') == Binary(Name("name"), Operator.LIKE, '%jim')
    assert Binary.parse(name__not_like='%jim') == Binary(Name("name"), Operator.NOT_LIKE, '%jim')
    assert Binary.parse(age__bt=[10, 60]) == Binary(Name("age"), Operator.BETWEEN, [10, 60])
    assert Binary.parse(age__not_bt=[10, 60]) == Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60])
    assert Binary.parse(age__gt=10) == Binary(Name("age"), Operator.GT, 10)
    assert Binary.parse(age__gte=10) == Binary(Name("age"), Operator.GTE, 10)
    assert Binary.parse(age__lt=30) == Binary(Name("age"), Operator.LT, 30)
    assert Binary.parse(age__lte=30) == Binary(Name("age"), Operator.LTE, 30)
    assert Binary.parse(users__username=Name("nickname", "users")) == Binary(Name("username", schema_name="users"), Operator.EQ, Name("nickname", "users"))
    assert Binary.parse(users__username__regex="^[b]abc") == Binary(Name("username", schema_name="users"), Operator.REGEX, "^[b]abc")
    assert Binary.parse(users__username__not_regex="^[b]abc") == Binary(Name("username", schema_name="users"), Operator.NOT_REGEX, "^[b]abc")


def test_eq(visitors):
    b = Binary(Name("name"), Operator.EQ, 'jim')
    assert visitors.mysql.sql(b) == "`name` = 'jim'"
    assert visitors.sqlite.sql(b) == "\"name\" = 'jim'"
    assert visitors.pg.sql(b) == "\"name\" = 'jim'"


def test_not_eq(visitors):
    b = Binary(Name("name"), Operator.NOT_EQ, 'jim')
    assert visitors.mysql.sql(b) == "`name` <> 'jim'"
    assert visitors.sqlite.sql(b) == "\"name\" <> 'jim'"
    assert visitors.pg.sql(b) == "\"name\" <> 'jim'"


def test_eq_with___(visitors):
    b = Binary(Name("name", schema_name="nick"), Operator.EQ, 'jim')
    assert visitors.mysql.sql(b) == "`nick`.`name` = 'jim'"
    assert visitors.sqlite.sql(b) == '"nick"."name" = \'jim\''
    assert visitors.pg.sql(b) == '"nick"."name" = \'jim\''


def test_is_null(visitors):
    b = Binary(Name("name"), Operator.IS, None)
    assert visitors.mysql.sql(b) == "`name` IS NULL"
    assert visitors.sqlite.sql(b) == "\"name\" IS NULL"
    assert visitors.pg.sql(b) == "\"name\" IS NULL"


def test_is_not_null(visitors):
    b = Binary(Name("name"), Operator.IS_NOT, None)
    assert visitors.mysql.sql(b) == "`name` IS NOT NULL"
    assert visitors.sqlite.sql(b) == "\"name\" IS NOT NULL"
    assert visitors.pg.sql(b) == "\"name\" IS NOT NULL"


def test_in(visitors):
    b = Binary(Name("name"), Operator.IN, ['jim', 'lucy', 'lily'])
    assert visitors.mysql.sql(b) == "`name` IN ('jim', 'lucy', 'lily')"
    assert visitors.sqlite.sql(b) == "\"name\" IN ('jim', 'lucy', 'lily')"
    assert visitors.pg.sql(b) == "\"name\" IN ('jim', 'lucy', 'lily')"


def test_not_in(visitors):
    b = Binary(Name("name"), Operator.NOT_IN, ['jim', 'lucy', 'lily'])
    assert visitors.mysql.sql(b) == "`name` NOT IN ('jim', 'lucy', 'lily')"
    assert visitors.sqlite.sql(b) == "\"name\" NOT IN ('jim', 'lucy', 'lily')"
    assert visitors.pg.sql(b) == "\"name\" NOT IN ('jim', 'lucy', 'lily')"


def test_like(visitors):
    b = Binary(Name("name"), Operator.LIKE, '%jim')
    assert visitors.mysql.sql(b) == "`name` LIKE '%jim'"
    assert visitors.sqlite.sql(b) == "\"name\" LIKE '%jim'"
    assert visitors.pg.sql(b) == "\"name\" LIKE '%jim'"


def test_not_like(visitors):
    b = Binary(Name("name"), Operator.NOT_LIKE, '%jim')
    assert visitors.mysql.sql(b) == "`name` NOT LIKE '%jim'"
    assert visitors.sqlite.sql(b) == "\"name\" NOT LIKE '%jim'"
    assert visitors.pg.sql(b) == "\"name\" NOT LIKE '%jim'"


def test_between(visitors):
    b = Binary(Name("age"), Operator.BETWEEN, [10, 60])
    assert visitors.mysql.sql(b) == "`age` BETWEEN 10 AND 60"
    assert visitors.sqlite.sql(b) == "\"age\" BETWEEN 10 AND 60"
    assert visitors.pg.sql(b) == "\"age\" BETWEEN 10 AND 60"


def test_between_err(visitors):
    with pytest.raises(ValueError, match='The "BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Binary.parse(age__bt=[10, 60, 10])

    with pytest.raises(ValueError, match='The "BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Binary(Name("age"), Operator.BETWEEN, [10, 60, 10])


def test_not_between(visitors):
    b = Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60])
    assert visitors.mysql.sql(b) == "`age` NOT BETWEEN 10 AND 60"
    assert visitors.sqlite.sql(b) == "\"age\" NOT BETWEEN 10 AND 60"
    assert visitors.pg.sql(b) == "\"age\" NOT BETWEEN 10 AND 60"


def test_not_between_err(visitors):
    with pytest.raises(ValueError, match='The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Binary.parse(age__not_bt=[10, 60, 10])

    with pytest.raises(ValueError, match='The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.'):
        Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60, 10])


def test_gt(visitors):
    b = Binary(Name("age"), Operator.GT, 10)
    assert visitors.mysql.sql(b) == "`age` > 10"
    assert visitors.sqlite.sql(b) == "\"age\" > 10"
    assert visitors.pg.sql(b) == "\"age\" > 10"


def test_gte(visitors):
    b = Binary(Name("age"), Operator.GTE, 10)
    assert visitors.mysql.sql(b) == "`age` >= 10"
    assert visitors.sqlite.sql(b) == "\"age\" >= 10"
    assert visitors.pg.sql(b) == "\"age\" >= 10"


def test_lt(visitors):
    b = Binary(Name("age"), Operator.LT, 30)
    assert visitors.mysql.sql(b) == "`age` < 30"
    assert visitors.sqlite.sql(b) == "\"age\" < 30"
    assert visitors.pg.sql(b) == "\"age\" < 30"


def test_lte(visitors):
    b = Binary(Name("age"), Operator.LTE, 30)
    assert visitors.mysql.sql(b) == "`age` <= 30"
    assert visitors.sqlite.sql(b) == "\"age\" <= 30"
    assert visitors.pg.sql(b) == "\"age\" <= 30"


def test_Name_value(visitors):
    b = Binary(Name("username", schema_name="users"), Operator.EQ, Name("nickname", "users"))
    assert visitors.mysql.sql(b) == '`users`.`username` = `users`.`nickname`'
    assert visitors.sqlite.sql(b) == '"users"."username" = "users"."nickname"'
    assert visitors.pg.sql(b) == '"users"."username" = "users"."nickname"'


def test_regex_value(visitors):
    b = Binary(Name("username", schema_name="users"), Operator.REGEX, "^[b]abc")
    assert visitors.mysql.sql(b) == "`users`.`username` REGEX '^[b]abc'"
    assert visitors.sqlite.sql(b) == "\"users\".\"username\" REGEX '^[b]abc'"
    assert visitors.pg.sql(b) == "\"users\".\"username\" REGEX '^[b]abc'"


def test_not_regex_value(visitors):
    b = Binary(Name("username", schema_name="users"), Operator.NOT_REGEX, "^[b]abc")
    assert visitors.mysql.sql(b) == "`users`.`username` NOT REGEX '^[b]abc'"
    assert visitors.sqlite.sql(b) == "\"users\".\"username\" NOT REGEX '^[b]abc'"
    assert visitors.pg.sql(b) == "\"users\".\"username\" NOT REGEX '^[b]abc'"


def test_equals(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2])
    b2 = Binary(Name("pk"), Operator.IN, [1, 2])
    assert b1 == b2


def test_not_equals(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2]) & Binary(Name("name"), Operator.EQ, "Lily")
    b2 = Binary(Name("pk"), Operator.IN, [1, 2])
    assert b1 != b2


def test_logic_and(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2])
    b2 = Binary(Name("name"), Operator.LIKE, '%jim')
    b3 = Binary(Name("age"), Operator.GT, 40)
    b = b1 & b2 & b3

    assert visitors.mysql.sql(b) == '`pk` IN (1, 2) AND `name` LIKE \'%jim\' AND `age` > 40'
    assert visitors.sqlite.sql(b) == '"pk" IN (1, 2) AND "name" LIKE \'%jim\' AND "age" > 40'
    assert visitors.pg.sql(b) == '"pk" IN (1, 2) AND "name" LIKE \'%jim\' AND "age" > 40'


def test_logic_or(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2])
    b2 = Binary(Name("name"), Operator.LIKE, '%jim')
    b3 = Binary(Name("age"), Operator.GT, 40)
    b = b1 | b2 | b3

    assert visitors.mysql.sql(b) == '`pk` IN (1, 2) OR `name` LIKE \'%jim\' OR `age` > 40'
    assert visitors.sqlite.sql(b) == '"pk" IN (1, 2) OR "name" LIKE \'%jim\' OR "age" > 40'
    assert visitors.pg.sql(b) == '"pk" IN (1, 2) OR "name" LIKE \'%jim\' OR "age" > 40'


def test_logic_invert_simple(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2])
    b2 = Binary(Name("name"), Operator.LIKE, '%jim')
    b3 = Binary(Name("age"), Operator.GT, 40)
    b4 = Binary(Name("gender"), Operator.IS, None)

    b = ~b1
    assert visitors.mysql.sql(b) == '`pk` NOT IN (1, 2)'
    assert visitors.sqlite.sql(b) == '"pk" NOT IN (1, 2)'
    assert visitors.pg.sql(b) == '"pk" NOT IN (1, 2)'

    b = ~b2
    assert visitors.mysql.sql(b) == '`name` NOT LIKE \'%jim\''
    assert visitors.sqlite.sql(b) == '"name" NOT LIKE \'%jim\''
    assert visitors.pg.sql(b) == '"name" NOT LIKE \'%jim\''

    b = ~b3
    assert visitors.mysql.sql(b) == 'NOT `age` > 40'
    assert visitors.sqlite.sql(b) == 'NOT "age" > 40'
    assert visitors.pg.sql(b) == 'NOT "age" > 40'

    b = ~b4
    assert visitors.mysql.sql(b) == '`gender` IS NOT NULL'
    assert visitors.sqlite.sql(b) == '"gender" IS NOT NULL'
    assert visitors.pg.sql(b) == '"gender" IS NOT NULL'


def test_logic_invert_simple2(visitors):
    b1 = Binary.parse(pk=1)
    b2 = Binary.parse(pk=2)

    b = ~b1 & ~b2
    assert visitors.mysql.sql(b) == 'NOT `pk` = 1 AND NOT `pk` = 2'
    assert visitors.sqlite.sql(b) == 'NOT "pk" = 1 AND NOT "pk" = 2'
    assert visitors.pg.sql(b) == 'NOT "pk" = 1 AND NOT "pk" = 2'

    b = ~(b1 & ~b2)
    assert visitors.mysql.sql(b) == 'NOT (`pk` = 1 AND NOT `pk` = 2)'
    assert visitors.sqlite.sql(b) == 'NOT ("pk" = 1 AND NOT "pk" = 2)'
    assert visitors.pg.sql(b) == 'NOT ("pk" = 1 AND NOT "pk" = 2)'


def test_logic_invert_complex(visitors):
    b1 = Binary(Name("pk"), Operator.IN, [1, 2])
    b2 = Binary(Name("name"), Operator.LIKE, '%jim')
    b3 = Binary(Name("age"), Operator.GT, 40)

    b = ~b1 & ~b2 & ~b3
    assert visitors.mysql.sql(b) == '`pk` NOT IN (1, 2) AND `name` NOT LIKE \'%jim\' AND NOT `age` > 40'
    assert visitors.sqlite.sql(b) == '"pk" NOT IN (1, 2) AND "name" NOT LIKE \'%jim\' AND NOT "age" > 40'
    assert visitors.pg.sql(b) == '"pk" NOT IN (1, 2) AND "name" NOT LIKE \'%jim\' AND NOT "age" > 40'

    b = ~b1 | ~b2 | ~b3
    assert visitors.mysql.sql(b) == '`pk` NOT IN (1, 2) OR `name` NOT LIKE \'%jim\' OR NOT `age` > 40'
    assert visitors.sqlite.sql(b) == '"pk" NOT IN (1, 2) OR "name" NOT LIKE \'%jim\' OR NOT "age" > 40'
    assert visitors.pg.sql(b) == '"pk" NOT IN (1, 2) OR "name" NOT LIKE \'%jim\' OR NOT "age" > 40'

    b = ~b1 & ~(b2 | ~b3)
    assert visitors.mysql.sql(b) == '`pk` NOT IN (1, 2) AND NOT (`name` LIKE \'%jim\' OR NOT `age` > 40)'
    assert visitors.sqlite.sql(b) == '"pk" NOT IN (1, 2) AND NOT ("name" LIKE \'%jim\' OR NOT "age" > 40)'
    assert visitors.pg.sql(b) == '"pk" NOT IN (1, 2) AND NOT ("name" LIKE \'%jim\' OR NOT "age" > 40)'


def test_logic_complex(visitors):
    b1 = Binary(Name("id"), Operator.EQ, 1)
    b2 = Binary(Name("name"), Operator.NOT_EQ, 'jim')
    b3 = Binary(Name("age"), Operator.GT, 40)
    b4 = Binary(Name("gender"), Operator.EQ, 'f')
    b5 = Binary(Name("score"), Operator.LT, 60)
    b6 = Binary(Name("country"), Operator.BETWEEN, ['USA', 'RP'])

    b = ~(b3 | b4 & (b5 | ~b6))
    assert visitors.mysql.sql(b) == """NOT (`age` > 40 OR `gender` = 'f' AND (`score` < 60 OR `country` NOT BETWEEN 'USA' AND 'RP'))"""
    assert visitors.sqlite.sql(b) == """NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))"""
    assert visitors.pg.sql(b) == """NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))"""

    b = (~b1 | b2) & ~(b3 | b4 & (b5 | ~b6))
    assert visitors.mysql.sql(b) == \
           """(NOT `id` = 1 OR `name` <> 'jim') AND NOT (`age` > 40 OR `gender` = 'f' AND (`score` < 60 OR `country` NOT BETWEEN 'USA' AND 'RP'))"""
    assert visitors.sqlite.sql(b) == \
           """(NOT "id" = 1 OR "name" <> 'jim') AND NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))"""
    assert visitors.pg.sql(b) == \
           """(NOT "id" = 1 OR "name" <> 'jim') AND NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))"""
