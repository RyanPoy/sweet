import unittest

from sweet.sequel import Operator
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.terms.binary import Binary
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestBinary(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_error_init(self):
        with self.assertRaises(ValueError) as ctx:
            Binary.parse(a=1, b=2)
        self.assertEqual("Only one parameter is allowed for construction.", str(ctx.exception))

    def test_init_from_parse(self):
        self.assertEqual(Binary(Name("name"), Operator.EQ, 'jim'), Binary.parse(name='jim'))
        self.assertEqual(Binary(Name("name"), Operator.NOT_EQ, 'jim'), Binary.parse(name__not='jim'))
        self.assertEqual(Binary(Name("name", schema_name="nick"), Operator.EQ, 'jim'), Binary.parse(nick__name='jim'))
        self.assertEqual(Binary(Name("name"), Operator.IS, None), Binary.parse(name=None))
        self.assertEqual(Binary(Name("name"), Operator.IS_NOT, None), Binary.parse(name__not=None))
        self.assertEqual(Binary(Name("name"), Operator.IN, ['jim', 'lucy', 'lily']), Binary.parse(name=['jim', 'lucy', 'lily']))
        self.assertEqual(Binary(Name("name"), Operator.NOT_IN, ['jim', 'lucy', 'lily']), Binary.parse(name__not=['jim', 'lucy', 'lily']))
        self.assertEqual(Binary(Name("name"), Operator.LIKE, '%jim'), Binary.parse(name__like='%jim'))
        self.assertEqual(Binary(Name("name"), Operator.NOT_LIKE, '%jim'), Binary.parse(name__not_like='%jim'))
        self.assertEqual(Binary(Name("age"), Operator.BETWEEN, [10, 60]), Binary.parse(age__bt=[10, 60]))
        self.assertEqual(Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60]), Binary.parse(age__not_bt=[10, 60]))
        self.assertEqual(Binary(Name("age"), Operator.GT, 10), Binary.parse(age__gt=10))
        self.assertEqual(Binary(Name("age"), Operator.GTE, 10), Binary.parse(age__gte=10))
        self.assertEqual(Binary(Name("age"), Operator.LT, 30), Binary.parse(age__lt=30))
        self.assertEqual(Binary(Name("age"), Operator.LTE, 30), Binary.parse(age__lte=30))
        self.assertEqual(Binary(Name("username", schema_name="users"), Operator.EQ, Name("nickname", "users")),
                         Binary.parse(users__username=Name("nickname", "users")))
        self.assertEqual(Binary(Name("username", schema_name="users"), Operator.REGEX, "^[b]abc"), Binary.parse(users__username__regex="^[b]abc"))
        self.assertEqual(Binary(Name("username", schema_name="users"), Operator.NOT_REGEX, "^[b]abc"), Binary.parse(users__username__not_regex="^[b]abc"))

    def test_eq(self):
        b = Binary(Name("name"), Operator.EQ, 'jim')
        self.assertEqual("`name` = 'jim'", self.mysql.sql(b))
        self.assertEqual("\"name\" = 'jim'", self.sqlite.sql(b))
        self.assertEqual("\"name\" = 'jim'", self.pg.sql(b))

    def test_not_eq(self):
        b = Binary(Name("name"), Operator.NOT_EQ, 'jim')
        self.assertEqual("`name` <> 'jim'", self.mysql.sql(b))
        self.assertEqual("\"name\" <> 'jim'", self.sqlite.sql(b))
        self.assertEqual("\"name\" <> 'jim'", self.pg.sql(b))

    def test_eq_with___(self):
        b = Binary(Name("name", schema_name="nick"), Operator.EQ, 'jim')
        self.assertEqual("`nick`.`name` = 'jim'", self.mysql.sql(b))
        self.assertEqual('"nick"."name" = \'jim\'', self.sqlite.sql(b))
        self.assertEqual('"nick"."name" = \'jim\'', self.pg.sql(b))

    def test_is_null(self):
        b = Binary(Name("name"), Operator.IS, None)
        self.assertEqual("`name` IS NULL", self.mysql.sql(b))
        self.assertEqual("\"name\" IS NULL", self.sqlite.sql(b))
        self.assertEqual("\"name\" IS NULL", self.pg.sql(b))

    def test_is_not_null(self):
        b = Binary(Name("name"), Operator.IS_NOT, None)
        self.assertEqual("`name` IS NOT NULL", self.mysql.sql(b))
        self.assertEqual("\"name\" IS NOT NULL", self.sqlite.sql(b))
        self.assertEqual("\"name\" IS NOT NULL", self.pg.sql(b))

    def test_in(self):
        b = Binary(Name("name"), Operator.IN, ['jim', 'lucy', 'lily'])
        self.assertEqual("`name` IN ('jim', 'lucy', 'lily')", self.mysql.sql(b))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", self.sqlite.sql(b))
        self.assertEqual("\"name\" IN ('jim', 'lucy', 'lily')", self.pg.sql(b))

    def test_not_in(self):
        b = Binary(Name("name"), Operator.NOT_IN, ['jim', 'lucy', 'lily'])
        self.assertEqual("`name` NOT IN ('jim', 'lucy', 'lily')", self.mysql.sql(b))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", self.sqlite.sql(b))
        self.assertEqual("\"name\" NOT IN ('jim', 'lucy', 'lily')", self.pg.sql(b))

    def test_like(self):
        b = Binary(Name("name"), Operator.LIKE, '%jim')
        self.assertEqual("`name` LIKE '%jim'", self.mysql.sql(b))
        self.assertEqual("\"name\" LIKE '%jim'", self.sqlite.sql(b))
        self.assertEqual("\"name\" LIKE '%jim'", self.pg.sql(b))

    def test_not_like(self):
        b = Binary(Name("name"), Operator.NOT_LIKE, '%jim')
        self.assertEqual("`name` NOT LIKE '%jim'", self.mysql.sql(b))
        self.assertEqual("\"name\" NOT LIKE '%jim'", self.sqlite.sql(b))
        self.assertEqual("\"name\" NOT LIKE '%jim'", self.pg.sql(b))

    def test_between(self):
        b = Binary(Name("age"), Operator.BETWEEN, [10, 60])
        self.assertEqual("`age` BETWEEN 10 AND 60", self.mysql.sql(b))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", self.sqlite.sql(b))
        self.assertEqual("\"age\" BETWEEN 10 AND 60", self.pg.sql(b))

    def test_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Binary.parse(age__bt=[10, 60, 10])
        self.assertEqual('The "BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

        with self.assertRaises(ValueError) as ctx:
            Binary(Name("age"), Operator.BETWEEN, [10, 60, 10])
        self.assertEqual('The "BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_not_between(self):
        b = Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60])
        self.assertEqual("`age` NOT BETWEEN 10 AND 60", self.mysql.sql(b))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", self.sqlite.sql(b))
        self.assertEqual("\"age\" NOT BETWEEN 10 AND 60", self.pg.sql(b))

    def test_not_between_err(self):
        with self.assertRaises(ValueError) as ctx:
            Binary.parse(age__not_bt=[10, 60, 10])
        self.assertEqual('The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

        with self.assertRaises(ValueError) as ctx:
            Binary(Name("age"), Operator.NOT_BETWEEN, [10, 60, 10])
        self.assertEqual('The "NOT BETWEEN" operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

    def test_gt(self):
        b = Binary(Name("age"), Operator.GT, 10)
        self.assertEqual("`age` > 10", self.mysql.sql(b))
        self.assertEqual("\"age\" > 10", self.sqlite.sql(b))
        self.assertEqual("\"age\" > 10", self.pg.sql(b))

    def test_gte(self):
        b = Binary(Name("age"), Operator.GTE, 10)
        self.assertEqual("`age` >= 10", self.mysql.sql(b))
        self.assertEqual("\"age\" >= 10", self.sqlite.sql(b))
        self.assertEqual("\"age\" >= 10", self.pg.sql(b))

    def test_lt(self):
        b = Binary(Name("age"), Operator.LT, 30)
        self.assertEqual("`age` < 30", self.mysql.sql(b))
        self.assertEqual("\"age\" < 30", self.sqlite.sql(b))
        self.assertEqual("\"age\" < 30", self.pg.sql(b))

    def test_lte(self):
        b = Binary(Name("age"), Operator.LTE, 30)
        self.assertEqual("`age` <= 30", self.mysql.sql(b))
        self.assertEqual("\"age\" <= 30", self.sqlite.sql(b))
        self.assertEqual("\"age\" <= 30", self.pg.sql(b))

    def test_Name_value(self):
        b = Binary(Name("username", schema_name="users"), Operator.EQ, Name("nickname", "users"))
        self.assertEqual('`users`.`username` = `users`.`nickname`', self.mysql.sql(b))
        self.assertEqual('"users"."username" = "users"."nickname"', self.sqlite.sql(b))
        self.assertEqual('"users"."username" = "users"."nickname"', self.pg.sql(b))

    def test_regex_value(self):
        b = Binary(Name("username", schema_name="users"), Operator.REGEX, "^[b]abc")
        self.assertEqual("`users`.`username` REGEX '^[b]abc'", self.mysql.sql(b))
        self.assertEqual("\"users\".\"username\" REGEX '^[b]abc'", self.sqlite.sql(b))
        self.assertEqual("\"users\".\"username\" REGEX '^[b]abc'", self.pg.sql(b))

    def test_not_regex_value(self):
        b = Binary(Name("username", schema_name="users"), Operator.NOT_REGEX, "^[b]abc")
        self.assertEqual("`users`.`username` NOT REGEX '^[b]abc'", self.mysql.sql(b))
        self.assertEqual("\"users\".\"username\" NOT REGEX '^[b]abc'", self.sqlite.sql(b))
        self.assertEqual("\"users\".\"username\" NOT REGEX '^[b]abc'", self.pg.sql(b))

    def test_equals(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2])
        b2 = Binary(Name("pk"), Operator.IN, [1, 2])
        self.assertEqual(b1, b2)

    def test_not_equals(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2]) & Binary(Name("name"), Operator.EQ, "Lily")
        b2 = Binary(Name("pk"), Operator.IN, [1, 2])
        self.assertNotEqual(b1, b2)

    def test_logic_and(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2])
        b2 = Binary(Name("name"), Operator.LIKE, '%jim')
        b3 = Binary(Name("age"), Operator.GT, 40)
        b = b1 & b2 & b3

        self.assertEqual('`pk` IN (1, 2) AND `name` LIKE \'%jim\' AND `age` > 40', self.mysql.sql(b))
        self.assertEqual('"pk" IN (1, 2) AND "name" LIKE \'%jim\' AND "age" > 40', self.sqlite.sql(b))
        self.assertEqual('"pk" IN (1, 2) AND "name" LIKE \'%jim\' AND "age" > 40', self.pg.sql(b))

    def test_logic_or(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2])
        b2 = Binary(Name("name"), Operator.LIKE, '%jim')
        b3 = Binary(Name("age"), Operator.GT, 40)
        b = b1 | b2 | b3

        self.assertEqual('`pk` IN (1, 2) OR `name` LIKE \'%jim\' OR `age` > 40', self.mysql.sql(b))
        self.assertEqual('"pk" IN (1, 2) OR "name" LIKE \'%jim\' OR "age" > 40', self.sqlite.sql(b))
        self.assertEqual('"pk" IN (1, 2) OR "name" LIKE \'%jim\' OR "age" > 40', self.pg.sql(b))

    def test_logic_invert_simple(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2])
        b2 = Binary(Name("name"), Operator.LIKE, '%jim')
        b3 = Binary(Name("age"), Operator.GT, 40)
        b4 = Binary(Name("gender"), Operator.IS, None)

        b = ~b1
        self.assertEqual('`pk` NOT IN (1, 2)', self.mysql.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2)', self.sqlite.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2)', self.pg.sql(b))

        b = ~b2
        self.assertEqual('`name` NOT LIKE \'%jim\'', self.mysql.sql(b))
        self.assertEqual('"name" NOT LIKE \'%jim\'', self.sqlite.sql(b))
        self.assertEqual('"name" NOT LIKE \'%jim\'', self.pg.sql(b))

        b = ~b3
        self.assertEqual('NOT `age` > 40', self.mysql.sql(b))
        self.assertEqual('NOT "age" > 40', self.sqlite.sql(b))
        self.assertEqual('NOT "age" > 40', self.pg.sql(b))

        b = ~b4
        self.assertEqual('`gender` IS NOT NULL', self.mysql.sql(b))
        self.assertEqual('"gender" IS NOT NULL', self.sqlite.sql(b))
        self.assertEqual('"gender" IS NOT NULL', self.pg.sql(b))

    def test_logic_invert_simple2(self):
        b1 = Binary.parse(pk=1)
        b2 = Binary.parse(pk=2)

        b = ~b1 & ~b2
        self.assertEqual('NOT `pk` = 1 AND NOT `pk` = 2', self.mysql.sql(b))
        self.assertEqual('NOT "pk" = 1 AND NOT "pk" = 2', self.sqlite.sql(b))
        self.assertEqual('NOT "pk" = 1 AND NOT "pk" = 2', self.pg.sql(b))

        b = ~(b1 & ~b2)
        self.assertEqual('NOT (`pk` = 1 AND NOT `pk` = 2)', self.mysql.sql(b))
        self.assertEqual('NOT ("pk" = 1 AND NOT "pk" = 2)', self.sqlite.sql(b))
        self.assertEqual('NOT ("pk" = 1 AND NOT "pk" = 2)', self.pg.sql(b))

    def test_logic_invert_complex(self):
        b1 = Binary(Name("pk"), Operator.IN, [1, 2])
        b2 = Binary(Name("name"), Operator.LIKE, '%jim')
        b3 = Binary(Name("age"), Operator.GT, 40)

        b = ~b1 & ~b2 & ~b3
        self.assertEqual('`pk` NOT IN (1, 2) AND `name` NOT LIKE \'%jim\' AND NOT `age` > 40', self.mysql.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) AND "name" NOT LIKE \'%jim\' AND NOT "age" > 40', self.sqlite.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) AND "name" NOT LIKE \'%jim\' AND NOT "age" > 40', self.pg.sql(b))

        b = ~b1 | ~b2 | ~b3
        self.assertEqual('`pk` NOT IN (1, 2) OR `name` NOT LIKE \'%jim\' OR NOT `age` > 40', self.mysql.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) OR "name" NOT LIKE \'%jim\' OR NOT "age" > 40', self.sqlite.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) OR "name" NOT LIKE \'%jim\' OR NOT "age" > 40', self.pg.sql(b))

        b = ~b1 & ~(b2 | ~b3)
        self.assertEqual('`pk` NOT IN (1, 2) AND NOT (`name` LIKE \'%jim\' OR NOT `age` > 40)', self.mysql.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) AND NOT ("name" LIKE \'%jim\' OR NOT "age" > 40)', self.sqlite.sql(b))
        self.assertEqual('"pk" NOT IN (1, 2) AND NOT ("name" LIKE \'%jim\' OR NOT "age" > 40)', self.pg.sql(b))

    def test_logic_complex(self):
        b1 = Binary(Name("id"), Operator.EQ, 1)
        b2 = Binary(Name("name"), Operator.NOT_EQ, 'jim')
        b3 = Binary(Name("age"), Operator.GT, 40)
        b4 = Binary(Name("gender"), Operator.EQ, 'f')
        b5 = Binary(Name("score"), Operator.LT, 60)
        b6 = Binary(Name("country"), Operator.BETWEEN, ['USA', 'RP'])

        b = ~(b3 | b4 & (b5 | ~b6))
        self.assertEqual(
            """NOT (`age` > 40 OR `gender` = 'f' AND (`score` < 60 OR `country` NOT BETWEEN 'USA' AND 'RP'))""",
            self.mysql.sql(b))
        self.assertEqual(
            """NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))""",
            self.sqlite.sql(b))
        self.assertEqual(
            """NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))""",
            self.pg.sql(b))

        b = (~b1 | b2) & ~(b3 | b4 & (b5 | ~b6))
        self.assertEqual(
            """(NOT `id` = 1 OR `name` <> 'jim') AND NOT (`age` > 40 OR `gender` = 'f' AND (`score` < 60 OR `country` NOT BETWEEN 'USA' AND 'RP'))""",
            self.mysql.sql(b))
        self.assertEqual(
            """(NOT "id" = 1 OR "name" <> 'jim') AND NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))""",
            self.sqlite.sql(b))
        self.assertEqual(
            """(NOT "id" = 1 OR "name" <> 'jim') AND NOT ("age" > 40 OR "gender" = 'f' AND ("score" < 60 OR "country" NOT BETWEEN 'USA' AND 'RP'))""",
            self.pg.sql(b))

if __name__ == '__main__':
    unittest.main()
