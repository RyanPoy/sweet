import unittest
from datetime import date

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms import fn, literal
from sweet.sequel.terms.name import Name
from sweet.sequel.terms.order import SortedIn
from sweet.sequel.terms.q import Q
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestSelectStatement(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()
        self.table_abc = Name("abc")
        self.table_efg = Name("efg")

    def test_empty_query(self):
        stmt = SelectStatement().from_(self.table_abc)
        self.assertEqual('SELECT * FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc"', self.pg.sql(stmt))

    def test_select__table_schema(self):
        stmt = SelectStatement().from_(Name("abc", "schema1"))
        self.assertEqual('SELECT * FROM `schema1`.`abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "schema1"."abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "schema1"."abc"', self.pg.sql(stmt))

    def test_select__distinct__single(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).distinct()
        self.assertEqual('SELECT DISTINCT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select__distinct__multi(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo"), Name("bar")).distinct()
        self.assertEqual('SELECT DISTINCT `foo`, `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo"))
        self.assertEqual('SELECT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column_with_alias(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column_and_table_alias_str(self):
        stmt = SelectStatement().from_(self.table_abc.as_("fizzbuzz")).select(Name("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` AS `fizzbuzz`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', self.pg.sql(stmt))

    def test_select_multiple_columns(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).select(Name("bar"))
        self.assertEqual('SELECT `foo`, `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_multiple_tables(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo", self.table_abc.name)) \
            .from_(self.table_efg).select(Name("bar", self.table_efg.name))
        self.assertEqual('SELECT `abc`.`foo`, `efg`.`bar` FROM `abc`, `efg`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', self.pg.sql(stmt))

    # def test_select_subquery(self):
    #     sub = SelectStatement().from_(self.table_abc)
    #     stmt = SelectStatement().from_(sub).select(Name("foo"), Name("bar"))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.mysql.sql(stmt))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.sqlite.sql(stmt))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.pg.sql(stmt))

    #     def test_select__multiple_subqueries(self):
    #         subquery0 = SelectStatement().from_(self.table_abc).select(Name("foo"))
    #         subquery1 = SelectStatement().from_(self.table_efg).select("bar")
    #         stmt = SelectStatement().from_(subquery0).from_(subquery1).select(subquery0.foo, subquery1.bar)
    #
    #         self.assertEqual(
    #             'SELECT "sq0"."foo","sq1"."bar" ' 'FROM (SELECT "foo" FROM "abc") "sq0",' '(SELECT "bar" FROM "efg") "sq1"',
    #             str(q),
    #         )
    #
    #     def test_select__nested_subquery(self):
    #         subquery0 = SelectStatement().from_(self.table_abc)
    #         subquery1 = SelectStatement().from_(subquery0).select(subquery0.foo, subquery0.bar)
    #         subquery2 = SelectStatement().from_(subquery1).select(subquery1.foo)
    #
    #         stmt = SelectStatement().from_(subquery2).select(subquery2.foo)
    #
    #         self.assertEqual(
    #             'SELECT "sq2"."foo" '
    #             'FROM (SELECT "sq1"."foo" '
    #             'FROM (SELECT "sq0"."foo","sq0"."bar" '
    #             'FROM (SELECT * FROM "abc") "sq0") "sq1") "sq2"',
    #             str(q),
    #         )
    #
    def test_select__no_table(self):
        stmt = SelectStatement().select(1, 2, 3)
        self.assertEqual("SELECT 1, 2, 3", self.mysql.sql(stmt))
        self.assertEqual("SELECT 1, 2, 3", self.sqlite.sql(stmt))
        self.assertEqual("SELECT 1, 2, 3", self.pg.sql(stmt))

    def test_select_then_add_table(self):
        stmt = SelectStatement().select(1, 2, 3).from_(self.table_abc).select("foo").select(Name("bar"))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_with_limit(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', self.pg.sql(stmt))

    def test_select_with_limit_zero(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).limit(0)
        self.assertEqual('SELECT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select_with_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).offset(10)
        self.assertEqual('SELECT `foo` FROM `abc` OFFSET 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', self.pg.sql(stmt))

    def test_select_with_limit_and_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).offset(10).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10 OFFSET 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', self.pg.sql(stmt))

    def test_select_with_force_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).force_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', self.pg.sql(stmt))

    def test_select_with_force_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).force_index(Name("egg"), Name("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `bacon`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', self.pg.sql(stmt))

    def test_select_with_force_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).force_index(Name("egg")).force_index(Name("spam"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `spam`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', self.pg.sql(stmt))

    def test_select_with_use_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).use_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', self.pg.sql(stmt))

    def test_select_with_use_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).use_index(Name("egg"), Name("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`, `bacon`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', self.pg.sql(stmt))

    def test_select_with_use_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).use_index(Name("egg")).use_index(Name("spam"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`, `spam`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")', self.pg.sql(stmt))

    def test_table_select_alias(self):
        stmt = SelectStatement().from_(self.table_abc).select(1)
        self.assertEqual('SELECT 1 FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT 1 FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT 1 FROM "abc"', self.pg.sql(stmt))

    def test_where_basic(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="foo")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 'foo'", self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'foo\'', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'foo\'', self.pg.sql(stmt))

        stmt = SelectStatement().from_(self.table_abc).where(foo=0)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 0", self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 0', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 0', self.pg.sql(stmt))

        stmt = SelectStatement().from_(self.table_abc).where(foo=True)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 1", self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 1', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 1', self.pg.sql(stmt))

        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2))
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = '2020-02-02'", self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\'', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\'', self.pg.sql(stmt))

        stmt = SelectStatement().from_(self.table_abc).where(foo=None)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` IS NULL", self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" IS NULL', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" IS NULL', self.pg.sql(stmt))

    def test_where_field_equals_for_update(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update()
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE', self.pg.sql(stmt))

    def test_where_field_equals_for_update_share(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(share=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SHARE', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE', self.pg.sql(stmt))

    def test_where_field_equals_for_update_nowait(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(nowait=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE NOWAIT', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT', self.pg.sql(stmt))

    def test_where_field_equals_for_update_skip(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(skip=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SKIP LOCKED', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED', self.pg.sql(stmt))

    def test_where_field_equals_for_update_of(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="bar").for_update(of=("abc",))
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'bar\' FOR UPDATE OF `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc"', self.pg.sql(stmt))

    def test_where_field_equals_for_update_skip_locked_and_of(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="bar").for_update(skip=True, of=("abc",))
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'bar\' FOR UPDATE OF `abc` SKIP LOCKED', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc" SKIP LOCKED', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc" SKIP LOCKED', self.pg.sql(stmt))

    def test_where_field_equals_for_multiple_tables(self):
        stmt = (SelectStatement().from_(self.table_abc)
                .join(self.table_efg).on(abc__id=Name("id", "efg"))
                .where(abc__foo=Name("bar", "efg"))
                )
        self.assertEqual('SELECT * FROM `abc` JOIN `efg` ON `abc`.`id` = `efg`.`id` WHERE `abc`.`foo` = `efg`.`bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"', self.pg.sql(stmt))

    def test_where_field_equals_where(self):
        stmt = SelectStatement().from_(self.table_abc).where(abc__foo=1).where(abc__bar=Name('baz', self.table_abc.name))
        self.assertEqual('SELECT * FROM `abc` WHERE `abc`.`foo` = 1 AND `abc`.`bar` = `abc`.`baz`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', self.pg.sql(stmt))

    def test_where_field_equals_where_not(self):
        stmt = SelectStatement().from_(self.table_abc).where(~Q(foo=1)).where(bar=Name('baz', self.table_abc.name))
        self.assertEqual('SELECT * FROM `abc` WHERE NOT `foo` = 1 AND `bar` = `abc`.`baz`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"', self.pg.sql(stmt))

    def test_where_single_quote(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="bar'foo")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 'bar''foo'", self.mysql.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'", self.sqlite.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'", self.pg.sql(stmt))

    def test_where_field_matches_regex(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo__regex="r^b")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` REGEX 'r^b'", self.mysql.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'", self.sqlite.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'", self.pg.sql(stmt))

    def test_ignore_empty_criterion_where(self):
        stmt = SelectStatement().from_(self.table_abc).where(Q())
        self.assertEqual("SELECT * FROM `abc`", self.mysql.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\"", self.sqlite.sql(stmt))
        self.assertEqual("SELECT * FROM \"abc\"", self.pg.sql(stmt))

    def test_select_with_force_index_and_where(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo")).where(foo="bar").force_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`) WHERE `foo` = \'bar\'', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\'', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\'', self.pg.sql(stmt))

    def test_group_by__single(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__multi(self):
        foo, bar = Name("foo"), Name("bar")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo, bar).select(foo, bar)
        self.assertEqual('SELECT `foo`, `bar` FROM `abc` GROUP BY `foo`, `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"', self.pg.sql(stmt))

    def test_group_by__count_star(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.count(literal.STAR))
        self.assertEqual('SELECT `foo`, COUNT(*) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__count_field(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.count(Name("bar")))
        self.assertEqual('SELECT `foo`, COUNT(`bar`) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__count_distinct(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.count(literal.STAR).distinct())
        self.assertEqual('SELECT `foo`, COUNT(DISTINCT *) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__sum_distinct(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.sum(Name("bar")).distinct())
        self.assertEqual('SELECT `foo`, SUM(DISTINCT `bar`) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__alias(self):
        bar = Name("bar").as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).select(fn.sum(Name("foo")), bar).group_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` GROUP BY `bar01`', self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"', self.pg.sql(stmt))

    def test_group_by__alias_with_join(self):
        table1 = Name("table1").as_("t1")
        bar = Name("bar", table1.alias).as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).join(table1).on(abc__id=Name("t_ref", table1.alias)).select(fn.sum(Name("foo")), bar).group_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `t1`.`bar` AS `bar01` FROM `abc` JOIN `table1` AS `t1` ON `abc`.`id` = `t1`.`t_ref` GROUP BY `bar01`',
                         self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"',
                         self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"',
                         self.pg.sql(stmt))

    def test_mysql_query_uses_backtick_quote_chars(self):
        stmt = SelectStatement().from_(self.table_abc).group_by(Name('foo')).select(Name('foo'))
        self.assertEqual('SELECT `foo` FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_having_greater_than(self):
        foo, bar = Name('foo'), Name('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo, fn.sum(bar)).group_by(foo).having(fn.sum(bar) > 1)

        self.assertEqual('SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING SUM(`bar`) > 1', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1', self.pg.sql(stmt))

    def test_having_and(self):
        foo, bar = Name('foo'), Name('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo, fn.sum(bar)).group_by(foo).having((fn.sum(bar) > 1) & (fn.sum(bar) < 100))
        self.assertEqual('SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING (SUM(`bar`) > 1 AND SUM(`bar`) < 100)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING (SUM("bar") > 1 AND SUM("bar") < 100)', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING (SUM("bar") > 1 AND SUM("bar") < 100)', self.pg.sql(stmt))

    def test_having_join_and_equality(self):
        abc_foo = Name('foo', self.table_abc.name)
        abc_buz = Name('buz', self.table_abc.name)
        efg_foo = Name('foo', self.table_efg.name)
        efg_bar = Name('bar', self.table_efg.name)

        stmt = (
            SelectStatement().from_(self.table_abc).join(self.table_efg)
            .on(abc__foo=efg_foo)
            .select(abc_foo, fn.sum(efg_bar), abc_buz)
            .group_by(abc_foo)
            .having(abc__buz="fiz")
            .having(fn.sum(efg_bar) > 100)
        )

        self.assertEqual('SELECT `abc`.`foo`, SUM(`efg`.`bar`), `abc`.`buz` FROM `abc` JOIN `efg` ON `abc`.`foo` = `efg`.`foo` '
                         'GROUP BY `abc`.`foo` HAVING `abc`.`buz` = \'fiz\' AND SUM(`efg`.`bar`) > 100', self.mysql.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", SUM("efg"."bar"), "abc"."buz" FROM "abc" JOIN "efg" ON "abc"."foo" = "efg"."foo" '
                         'GROUP BY "abc"."foo" HAVING "abc"."buz" = \'fiz\' AND SUM("efg"."bar") > 100', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", SUM("efg"."bar"), "abc"."buz" FROM "abc" JOIN "efg" ON "abc"."foo" = "efg"."foo" '
                         'GROUP BY "abc"."foo" HAVING "abc"."buz" = \'fiz\' AND SUM("efg"."bar") > 100', self.pg.sql(stmt))

    def test_order_by__single_field(self):
        stmt = SelectStatement().from_(self.table_abc).order_by(Name("foo")).select(Name("foo"))
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.pg.sql(stmt))

    def test_order_by__multi_fields(self):
        foo, bar = Name("foo"), Name("bar")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, bar).select(foo, bar)
        self.assertEqual('SELECT `foo`, `bar` FROM `abc` ORDER BY `foo`, `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"', self.pg.sql(stmt))

    def test_order_by_asc(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, sorted_in=SortedIn.ASC).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo` ASC', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" ASC', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" ASC', self.pg.sql(stmt))

    def test_order_by_desc(self):
        foo = Name("foo")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, sorted_in=SortedIn.DESC).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo` DESC', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" DESC', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" DESC', self.pg.sql(stmt))

    def test_order_by__alias(self):
        bar = Name("bar").as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).select(fn.sum(Name("foo")), bar).order_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` ORDER BY `bar01`', self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"', self.pg.sql(stmt))

    def test_table_field(self):
        bar = Name("foo").as_("bar")
        stmt = SelectStatement().from_(self.table_abc).select(bar)
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.pg.sql(stmt))

    def test_table_field__multi(self):
        stmt = SelectStatement().from_(self.table_abc).select(Name("foo").as_("bar"), Name("fiz").as_("buz"))
        self.assertEqual('SELECT `foo` AS `bar`, `fiz` AS `buz` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar", "fiz" AS "buz" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar", "fiz" AS "buz" FROM "abc"', self.pg.sql(stmt))

    # def test_arithmetic_function(self):
    #     """ @todo: support arithmetic """
    #     stmt = SelectStatement().from_(self.table_abc).select((self.t.foo + self.t.bar).as_("biz"))
    #     self.assertEqual('SELECT "foo"+"bar" "biz" FROM "abc"', self.mysql.sql(stmt))

    def test_alias_functions(self):
        stmt = SelectStatement().from_(self.table_abc).select(fn.count(literal.STAR).as_("foo"))
        self.assertEqual('SELECT COUNT(*) AS `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT COUNT(*) AS "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT COUNT(*) AS "foo" FROM "abc"', self.pg.sql(stmt))

    def test_alias_function_using_as_nested(self):
        """ We don't show aliases of fields that are arguments of a function. """
        stmt = SelectStatement().from_(self.table_abc).select(fn.sqrt(fn.count(literal.STAR).as_("foo")).as_("bar"))
        self.assertEqual('SELECT SQRT(COUNT(*)) AS `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT SQRT(COUNT(*)) AS "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT SQRT(COUNT(*)) AS "bar" FROM "abc"', self.pg.sql(stmt))

    def test_alias_in__group_by(self):
        foo = Name('foo').as_('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo).group_by(foo)
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` GROUP BY `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" GROUP BY "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" GROUP BY "bar"', self.pg.sql(stmt))

    def test_alias_in__order_by(self):
        foo = Name('foo').as_('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo).order_by(foo)
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` ORDER BY `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" ORDER BY "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" ORDER BY "bar"', self.pg.sql(stmt))

    def test_alias_ignored__in_value(self):
        foo = Name('foo').as_('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo).where(username=foo)
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` WHERE `username` = `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" WHERE "username" = "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" WHERE "username" = "foo"', self.pg.sql(stmt))

    def test_select__multiple_tables(self):
        table_abc = Name("abc").as_("t0")
        table_efg = Name("efg").as_("t1")
        foo = Name('foo', table_abc)
        bar = Name('bar', table_efg)
        stmt = SelectStatement().from_(table_abc).select(foo).from_(table_efg).select(bar)
        self.assertEqual('SELECT `t0`.`foo`, `t1`.`bar` FROM `abc` AS `t0`, `efg` AS `t1`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "t0"."foo", "t1"."bar" FROM "abc" AS "t0", "efg" AS "t1"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "t0"."foo", "t1"."bar" FROM "abc" AS "t0", "efg" AS "t1"', self.pg.sql(stmt))

    def test_use_aliases_in__group_by_and_order_by(self):
        table_abc = Name("abc").as_("t0")
        my_foo = Name("foo", table_abc.alias).as_("my_foo")
        bar = Name("bar", table_abc.alias)
        stmt = SelectStatement().from_(table_abc).select(my_foo, bar).group_by(my_foo).order_by(my_foo)
        self.assertEqual('SELECT `t0`.`foo` AS `my_foo`, `t0`.`bar` FROM `abc` AS `t0` GROUP BY `my_foo` ORDER BY `my_foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "t0"."foo" AS "my_foo", "t0"."bar" FROM "abc" AS "t0" GROUP BY "my_foo" ORDER BY "my_foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "t0"."foo" AS "my_foo", "t0"."bar" FROM "abc" AS "t0" GROUP BY "my_foo" ORDER BY "my_foo"', self.pg.sql(stmt))

    def test_table_with_schema_and_alias(self):
        table = Name("abc", schema_name="schema").as_("alias")
        stmt = SelectStatement().from_(table)
        self.assertEqual('SELECT * FROM `schema`.`abc` AS `alias`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "schema"."abc" AS "alias"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "schema"."abc" AS "alias"', self.pg.sql(stmt))

    def test_extraneous_quotes(self):
        t1 = Name("table1").as_("t1")
        t2 = Name("table2").as_("t2")
        stmt = SelectStatement().from_(t1).join(t2).on(t1__value__bt=(Name("start", t2), Name("end", t2))).select(Name("value", t1))
        self.assertEqual('SELECT `t1`.`value` FROM `table1` AS `t1` JOIN `table2` AS `t2` ON `t1`.`value` BETWEEN `t2`.`start` AND `t2`.`end`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "t1"."value" FROM "table1" AS "t1" JOIN "table2" AS "t2" ON "t1"."value" BETWEEN "t2"."start" AND "t2"."end"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "t1"."value" FROM "table1" AS "t1" JOIN "table2" AS "t2" ON "t1"."value" BETWEEN "t2"."start" AND "t2"."end"', self.pg.sql(stmt))


# class SubqueryTests(unittest.TestCase):
#     maxDiff = None
#
#     table_abc, table_efg, table_hij = Tables("abc", "efg", "hij")
#
#     def test_where__in(self):
#         stmt = (
#             SelectStatement().from_(self.table_abc)
#
#             .where(
#                 self.table_abc.foo.isin(
#                     SelectStatement().from_(self.table_efg).select(self.table_efg.foo).where(self.table_efg.bar == 0)
#                 )
#             )
#         )
#
#         self.assertEqual(
#             'SELECT * FROM "abc" WHERE "foo" IN (SELECT "foo" FROM "efg" WHERE "bar"=0)',
#             str(q),
#         )
#
#     def test_where__in_nested(self):
#         stmt = SelectStatement().from_(self.table_abc).where(self.table_abc.foo).isin(self.table_efg)
#
#         self.assertEqual('SELECT * FROM "abc" WHERE "foo" IN (SELECT * FROM "efg")', self.mysql.sql(stmt))
#
#     def test_join(self):
#         subquery = SelectStatement().from_("efg").select("fiz", "buz").where(F("buz") == 0)
#
#         stmt = (
#             SelectStatement().from_(self.table_abc)
#             .join(subquery)
#             .on(self.table_abc.bar == subquery.buz)
#             .select(self.table_abc.foo, subquery.fiz)
#         )
#
#         self.assertEqual(
#             'SELECT "abc"."foo","sq0"."fiz" FROM "abc" '
#             'JOIN (SELECT "fiz","buz" FROM "efg" WHERE "buz"=0) "sq0" '
#             'ON "abc"."bar"="sq0"."buz"',
#             str(q),
#         )
#
#     def test_select_subquery(self):
#         substmt = SelectStatement().from_(self.table_efg).select("fizzbuzz").where(self.table_efg.id == 1)
#
#         stmt = SelectStatement().from_(self.table_abc).select("foo", "bar").select(subq)
#
#         self.assertEqual(
#             'SELECT "foo","bar",(SELECT "fizzbuzz" FROM "efg" WHERE "id"=1) ' 'FROM "abc"',
#             str(q),
#         )
#
#     def test_select_subquery_with_alias(self):
#         substmt = SelectStatement().from_(self.table_efg).select("fizzbuzz").where(self.table_efg.id == 1)
#
#         stmt = SelectStatement().from_(self.table_abc).select("foo", "bar").select(subq.as_("sq"))
#
#         self.assertEqual(
#             'SELECT "foo","bar",(SELECT "fizzbuzz" FROM "efg" WHERE "id"=1) "sq" ' 'FROM "abc"',
#             str(q),
#         )
#
#     def test_where__equality(self):
#         subquery = SelectStatement().from_("efg").select("fiz").where(F("buz") == 0)
#         query = (
#             SelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo, self.table_abc.bar)
#             .where(self.table_abc.bar == subquery)
#         )
#
#         self.assertEqual(
#             'SELECT "foo","bar" FROM "abc" ' 'WHERE "bar"=(SELECT "fiz" FROM "efg" WHERE "buz"=0)',
#             str(query),
#         )
#
#     def test_select_from_nested_query(self):
#         subquery = SelectStatement().from_(self.table_abc).select(
#             self.table_abc.foo,
#             self.table_abc.bar,
#             (self.table_abc.fizz + self.table_abc.buzz).as_("fizzbuzz"),
#         )
#
#         query = SelectStatement().from_(subquery).select(subquery.foo, subquery.bar, subquery.fizzbuzz)
#
#         self.assertEqual(
#             'SELECT "sq0"."foo","sq0"."bar","sq0"."fizzbuzz" '
#             "FROM ("
#             'SELECT "foo","bar","fizz"+"buzz" "fizzbuzz" '
#             'FROM "abc"'
#             ') "sq0"',
#             str(query),
#         )
#
#     def test_select_from_nested_query_with_join(self):
#         subquery1 = (
#             SelectStatement().from_(self.table_abc)
#             .select(
#                 self.table_abc.foo,
#                 fn.Sum(self.table_abc.fizz + self.table_abc.buzz).as_("fizzbuzz"),
#             )
#             .group_by(self.table_abc.foo)
#         )
#
#         subquery2 = SelectStatement().from_(self.table_efg).select(
#             self.table_efg.foo.as_("foo_two"),
#             self.table_efg.bar,
#         )
#
#         query = (
#             SelectStatement().from_(subquery1)
#             .select(subquery1.foo, subquery1.fizzbuzz)
#             .join(subquery2)
#             .on(subquery1.foo == subquery2.foo_two)
#             .select(subquery2.foo_two, subquery2.bar)
#         )
#
#         self.assertEqual(
#             "SELECT "
#             '"sq0"."foo","sq0"."fizzbuzz",'
#             '"sq1"."foo_two","sq1"."bar" '
#             "FROM ("
#             "SELECT "
#             '"foo",SUM("fizz"+"buzz") "fizzbuzz" '
#             'FROM "abc" '
#             'GROUP BY "foo"'
#             ') "sq0" JOIN ('
#             "SELECT "
#             '"foo" "foo_two","bar" '
#             'FROM "efg"'
#             ') "sq1" ON "sq0"."foo"="sq1"."foo_two"',
#             str(query),
#         )
#
#     def test_from_subquery_without_alias(self):
#         subquery = SelectStatement().from_(self.table_efg).select(
#             self.table_efg.base_id.as_("x"), self.table_efg.fizz, self.table_efg.buzz
#         )
#
#         test_query = SelectStatement().from_(subquery).select(subquery.x, subquery.fizz, subquery.buzz)
#
#         self.assertEqual(
#             'SELECT "sq0"."x","sq0"."fizz","sq0"."buzz" '
#             "FROM ("
#             'SELECT "base_id" "x","fizz","buzz" FROM "efg"'
#             ') "sq0"',
#             str(test_query),
#         )
#
#     def test_join_query_with_alias(self):
#         subquery = (
#             SelectStatement().from_(self.table_efg)
#             .select(
#                 self.table_efg.base_id.as_("x"),
#                 self.table_efg.fizz,
#                 self.table_efg.buzz,
#             )
#             .as_("subq")
#         )
#
#         test_query = SelectStatement().from_(subquery).select(subquery.x, subquery.fizz, subquery.buzz)
#
#         self.assertEqual(
#             'SELECT "subq"."x","subq"."fizz","subq"."buzz" '
#             "FROM ("
#             'SELECT "base_id" "x","fizz","buzz" FROM "efg"'
#             ') "subq"',
#             str(test_query),
#         )
#
#     def test_with(self):
#         sub_query = SelectStatement().from_(self.table_efg).select("fizz")
#         test_query = SelectStatement().with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
#
#         self.assertEqual(
#             'WITH an_alias AS (SELECT "fizz" FROM "efg") SELECT * FROM an_alias',
#             str(test_query),
#         )
#
#     def test_join_with_with(self):
#         sub_query = SelectStatement().from_(self.table_efg).select("fizz")
#         test_query = (
#             SelectStatement().with_(sub_query, "an_alias")
#             .from_(self.table_abc)
#             .join(AliasedQuery("an_alias"))
#             .on(AliasedQuery("an_alias").fizz == self.table_abc.buzz)
#
#         )
#         self.assertEqual(
#             'WITH an_alias AS (SELECT "fizz" FROM "efg") '
#             'SELECT * FROM "abc" JOIN an_alias ON "an_alias"."fizz"="abc"."buzz"',
#             str(test_query),
#         )
#
#     def test_select_from_with_returning(self):
#         sub_query = SelectStatement().into(self.table_abc).insert(1).returning('*')
#         test_query = SelectStatement().with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
#         self.assertEqual(
#             'WITH an_alias AS (INSERT INTO "abc" VALUES (1) RETURNING *) SELECT * FROM an_alias', str(test_query)
#         )
#
#

if __name__ == '__main__':
    unittest.main()
