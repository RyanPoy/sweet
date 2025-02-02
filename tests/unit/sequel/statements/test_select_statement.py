import unittest
from datetime import date

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms import fn
from sweet.sequel.terms.name import ColumnName, Name
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
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).distinct()
        self.assertEqual('SELECT DISTINCT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select__distinct__multi(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo"), ColumnName("bar")).distinct()
        self.assertEqual('SELECT DISTINCT `foo`, `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo"))
        self.assertEqual('SELECT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column_with_alias(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_single_column_and_table_alias_str(self):
        stmt = SelectStatement().from_(self.table_abc.as_("fizzbuzz")).select(ColumnName("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` AS `fizzbuzz`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', self.pg.sql(stmt))

    def test_select_multiple_columns(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).select(ColumnName("bar"))
        self.assertEqual('SELECT `foo`, `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_multiple_tables(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo", self.table_abc.value)) \
            .from_(self.table_efg).select(ColumnName("bar", self.table_efg.value))
        self.assertEqual('SELECT `abc`.`foo`, `efg`.`bar` FROM `abc`, `efg`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', self.pg.sql(stmt))

    # def test_select_subquery(self):
    #     sub = SelectStatement().from_(self.table_abc)
    #     stmt = SelectStatement().from_(sub).select(ColumnName("foo"), ColumnName("bar"))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.mysql.sql(stmt))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.sqlite.sql(stmt))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', self.pg.sql(stmt))

    #     def test_select__multiple_subqueries(self):
    #         subquery0 = SelectStatement().from_(self.table_abc).select(ColumnName("foo"))
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
        stmt = SelectStatement().select(1, 2, 3).from_(self.table_abc).select("foo").select(ColumnName("bar"))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', `bar` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', self.pg.sql(stmt))

    def test_select_with_limit(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', self.pg.sql(stmt))

    def test_select_with_limit_zero(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).limit(0)
        self.assertEqual('SELECT `foo` FROM `abc`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc"', self.pg.sql(stmt))

    def test_select_with_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).offset(10)
        self.assertEqual('SELECT `foo` FROM `abc` OFFSET 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', self.pg.sql(stmt))

    def test_select_with_limit_and_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).offset(10).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10 OFFSET 10', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', self.pg.sql(stmt))

    def test_select_with_force_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', self.pg.sql(stmt))

    def test_select_with_force_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(Name("egg"), Name("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `bacon`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', self.pg.sql(stmt))

    def test_select_with_force_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(Name("egg")).force_index(Name("spam"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `spam`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', self.pg.sql(stmt))

    def test_select_with_use_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', self.pg.sql(stmt))

    def test_select_with_use_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(Name("egg"), Name("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`, `bacon`)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', self.pg.sql(stmt))

    def test_select_with_use_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(Name("egg")).use_index(Name("spam"))
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
                .join(self.table_efg).on(abc__id=ColumnName("id", "efg"))
                .where(abc__foo=ColumnName("bar", "efg"))
                )
        self.assertEqual('SELECT * FROM `abc` JOIN `efg` ON `abc`.`id` = `efg`.`id` WHERE `abc`.`foo` = `efg`.`bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"', self.pg.sql(stmt))

    def test_where_field_equals_where(self):
        stmt = SelectStatement().from_(self.table_abc).where(abc__foo=1).where(abc__bar=ColumnName('baz', self.table_abc))
        self.assertEqual('SELECT * FROM `abc` WHERE `abc`.`foo` = 1 AND `abc`.`bar` = `abc`.`baz`', self.mysql.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', self.pg.sql(stmt))

    def test_where_field_equals_where_not(self):
        stmt = SelectStatement().from_(self.table_abc).where(~Q(foo=1)).where(bar=ColumnName('baz', self.table_abc))
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
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).where(foo="bar").force_index(Name("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`) WHERE `foo` = \'bar\'', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\'', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\'', self.pg.sql(stmt))

    def test_group_by__single(self):
        foo = ColumnName("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__multi(self):
        foo, bar = ColumnName("foo"), ColumnName("bar")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo, bar).select(foo, bar)
        self.assertEqual('SELECT `foo`, `bar` FROM `abc` GROUP BY `foo`, `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"', self.pg.sql(stmt))

    def test_group_by__count_star(self):
        foo = ColumnName("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.count("*"))
        self.assertEqual('SELECT `foo`, COUNT(*) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__count_field(self):
        stmt = SelectStatement().from_(self.table_abc).group_by("foo").select(ColumnName("foo"), fn.count("bar"))
        self.assertEqual('SELECT `foo`, COUNT(`bar`) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__count_distinct(self):
        stmt = SelectStatement().from_(self.table_abc).group_by("foo").select(ColumnName("foo"), fn.count("*").distinct())
        self.assertEqual('SELECT `foo`, COUNT(DISTINCT *) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__sum_distinct(self):
        foo = ColumnName("foo")
        stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, fn.sum("bar").distinct())
        self.assertEqual('SELECT `foo`, SUM(DISTINCT `bar`) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    # def test_group_by__sum_filter(self):
    #     """ for ClickHouse, TimescaleDB, PostgreSQL 9.4+ """
    #     foo, bar = ColumnName("foo"), ColumnName("bar")
    #     stmt = SelectStatement().from_(self.table_abc).group_by(foo).select(foo, bar).where(id=1, cid__gt=2)
    #     self.assertEqual('SELECT "foo", SUM("bar") FILTER(WHERE "id"=1 AND "cid">2) FROM "abc" GROUP BY "foo"', self.mysql.sql(stmt))
    #     self.assertEqual('SELECT "foo", SUM("bar") FILTER(WHERE "id"=1 AND "cid">2) FROM "abc" GROUP BY "foo"', self.mysql.sql(stmt))
    #     self.assertEqual('SELECT "foo", SUM("bar") FILTER(WHERE "id"=1 AND "cid">2) FROM "abc" GROUP BY "foo"', self.mysql.sql(stmt))

    def test_group_by__str(self):
        stmt = SelectStatement().from_(self.table_abc).group_by("foo").select(ColumnName("foo"), fn.count("*").distinct())
        self.assertEqual('SELECT `foo`, COUNT(DISTINCT *) FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_group_by__int(self):
        stmt = SelectStatement().from_(self.table_abc).group_by(1).select(ColumnName("foo"), fn.count("*").distinct())
        self.assertEqual('SELECT `foo`, COUNT(DISTINCT *) FROM `abc` GROUP BY 1', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY 1', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY 1', self.pg.sql(stmt))

    def test_group_by__alias(self):
        bar = ColumnName("bar").as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).select(fn.sum("foo"), bar).group_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` GROUP BY `bar01`', self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"', self.pg.sql(stmt))

    def test_group_by__alias_with_join(self):
        table1 = Name("table1").as_("t1")
        bar = ColumnName("bar", table1.alias).as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).join(table1).on(abc__id=ColumnName("t_ref", table1.alias)).select(fn.sum("foo"), bar).group_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `t1`.`bar` AS `bar01` FROM `abc` JOIN `table1` AS `t1` ON `abc`.`id` = `t1`.`t_ref` GROUP BY `bar01`',
                         self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"',
                         self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"',
                         self.pg.sql(stmt))

    def test_mysql_query_uses_backtick_quote_chars(self):
        stmt = SelectStatement().from_(self.table_abc).group_by(ColumnName('foo')).select(ColumnName('foo'))
        self.assertEqual('SELECT `foo` FROM `abc` GROUP BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.pg.sql(stmt))

    def test_having_greater_than(self):
        foo, bar = ColumnName('foo'), ColumnName('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo, fn.sum(bar)).group_by(foo).having(fn.sum(bar) > 1)

        self.assertEqual('SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING SUM(`bar`) > 1', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1', self.pg.sql(stmt))

    def test_having_and(self):
        foo, bar = ColumnName('foo'), ColumnName('bar')
        stmt = SelectStatement().from_(self.table_abc).select(foo, fn.sum(bar)).group_by(foo).having((fn.sum(bar) > 1) & (fn.sum(bar) < 100))
        self.assertEqual('SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING (SUM(`bar`) > 1 AND SUM(`bar`) < 100)', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING (SUM("bar") > 1 AND SUM("bar") < 100)', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING (SUM("bar") > 1 AND SUM("bar") < 100)', self.pg.sql(stmt))

    def test_having_join_and_equality(self):
        abc_foo = ColumnName('foo', self.table_abc)
        abc_buz = ColumnName('buz', self.table_abc)
        efg_foo = ColumnName('foo', self.table_efg)
        efg_bar = ColumnName('bar', self.table_efg)

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
        stmt = SelectStatement().from_(self.table_abc).order_by(ColumnName("foo")).select(ColumnName("foo"))
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.pg.sql(stmt))

    def test_order_by__multi_fields(self):
        foo, bar = ColumnName("foo"), ColumnName("bar")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, bar).select(foo, bar)
        self.assertEqual('SELECT `foo`, `bar` FROM `abc` ORDER BY `foo`, `bar`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"', self.pg.sql(stmt))

    def test_order_by__single_str(self):
        stmt = SelectStatement().from_(self.table_abc).order_by("foo").select(ColumnName("foo"))
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo`', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.pg.sql(stmt))

    def test_order_by_asc(self):
        foo = ColumnName("foo")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, sorted_in=SortedIn.ASC).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo` ASC', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" ASC', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" ASC', self.pg.sql(stmt))

    def test_order_by_desc(self):
        foo = ColumnName("foo")
        stmt = SelectStatement().from_(self.table_abc).order_by(foo, sorted_in=SortedIn.DESC).select(foo)
        self.assertEqual('SELECT `foo` FROM `abc` ORDER BY `foo` DESC', self.mysql.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" DESC', self.sqlite.sql(stmt))
        self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" DESC', self.pg.sql(stmt))

    def test_order_by__alias(self):
        bar = ColumnName("bar").as_("bar01")
        stmt = SelectStatement().from_(self.table_abc).select(fn.sum(ColumnName("foo")), bar).order_by(bar)
        self.assertEqual('SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` ORDER BY `bar01`', self.mysql.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"', self.sqlite.sql(stmt))
        self.assertEqual('SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"', self.pg.sql(stmt))

# class AliasTests(unittest.TestCase):
#     t = Table("abc")
#
#     def test_table_field(self):
#         stmt = SelectStatement().from_(self.table_abc).select(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" "bar" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_table_field__multi(self):
#         stmt = SelectStatement().from_(self.table_abc).select(self.t.foo.as_("bar"), self.t.fiz.as_("buz"))
#
#         self.assertEqual('SELECT "foo" "bar","fiz" "buz" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_arithmetic_function(self):
#         stmt = SelectStatement().from_(self.table_abc).select((self.t.foo + self.t.bar).as_("biz"))
#
#         self.assertEqual('SELECT "foo"+"bar" "biz" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_functions_using_as(self):
#         stmt = SelectStatement().from_(self.table_abc).select(fn.Count("*").as_("foo"))
#
#         self.assertEqual('SELECT COUNT(*) "foo" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_functions_using_constructor_param(self):
#         stmt = SelectStatement().from_(self.table_abc).select(fn.Count("*", alias="foo"))
#
#         self.assertEqual('SELECT COUNT(*) "foo" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_function_using_as_nested(self):
#         """
#         We don't show aliases of fields that are arguments of a function.
#         """
#         stmt = SelectStatement().from_(self.table_abc).select(fn.Sqrt(fn.Count("*").as_("foo")).as_("bar"))
#
#         self.assertEqual('SELECT SQRT(COUNT(*)) "bar" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_functions_using_constructor_param_nested(self):
#         """
#         We don't show aliases of fields that are arguments of a function.
#         """
#         stmt = SelectStatement().from_(self.table_abc).select(fn.Sqrt(fn.Count("*", alias="foo"), alias="bar"))
#
#         self.assertEqual('SELECT SQRT(COUNT(*)) "bar" FROM "abc"', self.mysql.sql(stmt))
#
#     def test_ignored_in_where(self):
#         stmt = SelectStatement().from_(self.table_abc).select(self.t.foo).where(self.t.foo.as_("bar") == 1)
#
#         self.assertEqual('SELECT "foo" FROM "abc" WHERE "foo"=1', self.mysql.sql(stmt))
#
#     def test_ignored_in_groupby(self):
#         stmt = SelectStatement().from_(self.table_abc).select(self.t.foo).group_by(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', self.mysql.sql(stmt))
#
#     def test_ignored_in_order_by(self):
#         stmt = SelectStatement().from_(self.table_abc).select(self.t.foo).order_by(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', self.mysql.sql(stmt))
#
#     def test_ignored_in_criterion(self):
#         c = self.t.foo.as_("bar") == 1
#
#         self.assertEqual('"foo"=1', str(c))
#
#     def test_ignored_in_criterion_comparison(self):
#         c = self.t.foo.as_("bar") == self.t.fiz.as_("buz")
#
#         self.assertEqual('"foo"="fiz"', str(c))
#
#     def test_ignored_in_field_inside_case(self):
#         stmt = SelectStatement().from_(self.table_abc).select(Case().when(self.t.foo == 1, "a").else_(self.t.bar.as_('"buz"')))
#
#         self.assertEqual('SELECT CASE WHEN "foo"=1 THEN \'a\' ELSE "bar" END FROM "abc"', self.mysql.sql(stmt))
#
#     def test_case_using_as(self):
#         stmt = SelectStatement().from_(self.table_abc).select(Case().when(self.t.foo == 1, "a").else_("b").as_("bar"))
#
#         self.assertEqual(
#             'SELECT CASE WHEN "foo"=1 THEN \'a\' ELSE \'b\' END "bar" FROM "abc"',
#             str(q),
#         )
#
#     def test_case_using_constructor_param(self):
#         stmt = SelectStatement().from_(self.table_abc).select(Case(alias="bar").when(self.t.foo == 1, "a").else_("b"))
#
#         self.assertEqual(
#             'SELECT CASE WHEN "foo"=1 THEN \'a\' ELSE \'b\' END "bar" FROM "abc"',
#             str(q),
#         )
#
#     def test_select__multiple_tables(self):
#         table_abc, table_efg = Table("abc", alias="q0"), Table("efg", alias="q1")
#
#         stmt = SelectStatement().from_(table_abc).select(table_abc.foo).from_(table_efg).select(table_efg.bar)
#
#         self.assertEqual('SELECT "q0"."foo","q1"."bar" FROM "abc" "q0","efg" "q1"', self.mysql.sql(stmt))
#
#     def test_use_aliases_in_groupby_and_order_by(self):
#         table_abc = Table("abc", alias="q0")
#
#         my_foo = table_abc.foo.as_("my_foo")
#         stmt = SelectStatement().from_(table_abc).select(my_foo, table_abc.bar).group_by(my_foo).order_by(my_foo)
#
#         self.assertEqual(
#             'SELECT "q0"."foo" "my_foo","q0"."bar" ' 'FROM "abc" "q0" ' 'GROUP BY "my_foo" ' 'ORDER BY "my_foo"',
#             str(q),
#         )
#
#     def test_table_with_schema_and_alias(self):
#         table = Table("abc", schema="schema", alias="alias")
#         self.assertEqual('"schema"."abc" "alias"', str(table))
#
#     def test_null_value_with_alias(self):
#         stmt = Query.select(NullValue().as_("abcdef"))
#
#         self.assertEqual('SELECT NULL "abcdef"', self.mysql.sql(stmt))
#
#
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
#         test_query = Query.with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
#
#         self.assertEqual(
#             'WITH an_alias AS (SELECT "fizz" FROM "efg") SELECT * FROM an_alias',
#             str(test_query),
#         )
#
#     def test_join_with_with(self):
#         sub_query = SelectStatement().from_(self.table_efg).select("fizz")
#         test_query = (
#             Query.with_(sub_query, "an_alias")
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
#         sub_query = PostgreSQLQuery.into(self.table_abc).insert(1).returning('*')
#         test_query = Query.with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
#         self.assertEqual(
#             'WITH an_alias AS (INSERT INTO "abc" VALUES (1) RETURNING *) SELECT * FROM an_alias', str(test_query)
#         )
#
#
# class QuoteTests(unittest.TestCase):
#     def test_extraneous_quotes(self):
#         t1 = Table("table1", alias="t1")
#         t2 = Table("table2", alias="t2")
#
#         query = SelectStatement().from_(t1).join(t2).on(t1.Value.between(t2.start, t2.end)).select(t1.value)
#
#         self.assertEqual(
#             "SELECT t1.value FROM table1 t1 " "JOIN table2 t2 ON t1.Value " "BETWEEN t2.start AND t2.end",
#             query.get_sql(quote_char=None),
#         )
#
# class PreWhereTests(WhereTests):
#     t = Table("abc")
#
#     def test_prewhere_field_equals(self):
#         stmt = SelectStatement().from_(self.table_abc).prewhere(self.t.foo == self.t.bar)
#         q2 = SelectStatement().from_(self.table_abc).prewhere(self.t.foo.eq(self.t.bar))
#
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar"', str(q1))
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar"', str(q2))
#
#     def test_where_and_prewhere(self):
#         stmt = SelectStatement().from_(self.table_abc).prewhere(self.t.foo == self.t.bar).where(self.t.foo == self.t.bar)
#
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar" WHERE "foo"="bar"', self.mysql.sql(stmt))
#
#


if __name__ == '__main__':
    unittest.main()
