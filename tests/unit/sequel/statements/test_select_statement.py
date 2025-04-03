import unittest
from datetime import date

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Count, Name, Sqrt, Sum
from sweet.sequel.terms.literal import STAR
from sweet.sequel.terms.order import SortedIn


def test_empty_query(visitors):
    stmt = SelectStatement().from_(Name("abc"))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc"'


def test_select__table_schema(visitors):
    stmt = SelectStatement().from_(Name("abc", schema_name="schema1"))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `schema1`.`abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "schema1"."abc"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "schema1"."abc"'


def test_select__distinct__single(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).distinct()
    assert visitors.mysql.sql(stmt) == 'SELECT DISTINCT `foo` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT DISTINCT "foo" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT DISTINCT "foo" FROM "abc"'


def test_select__distinct__multi(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo"), Name("bar")).distinct()
    assert visitors.mysql.sql(stmt) == 'SELECT DISTINCT `foo`, `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT DISTINCT "foo", "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT DISTINCT "foo", "bar" FROM "abc"'


def test_select_single_column(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc"'


def test_select_single_column_with_alias(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo").as_("bar"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc"'


def test_select_single_column_and_table_alias_str(visitors):
    stmt = SelectStatement().from_(Name("abc").as_("fizzbuzz")).select(Name("foo").as_("bar"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc` AS `fizzbuzz`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"'


def test_select_multiple_columns(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).select(Name("bar"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", "bar" FROM "abc"'


def test_select_multiple_tables(visitors):
    stmt = (SelectStatement().from_(Name("abc")).select(Name("foo", schema_name=Name("abc").name))
            .from_(Name("efg")).select(Name("bar", schema_name=Name("efg").name)))
    assert visitors.mysql.sql(stmt) == 'SELECT `abc`.`foo`, `efg`.`bar` FROM `abc`, `efg`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"'
    assert visitors.pg.sql(stmt) == 'SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"'


# def test_select_subquery(visitors):
#     sub = SelectStatement().from_(Name("abc"))
#     stmt = SelectStatement().from_(sub).select(Name("foo"), Name("bar"))
# visitors.mysql.sql(stmt) ==     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"'
# visitors.sqlite.sql(stmt) ==     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"'
# visitors.pg.sql(stmt) ==     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"'

#     def test_select__multiple_subqueries(visitors):
#         subquery0 = SelectStatement().from_(Name("abc")).select(Name("foo"))
#         subquery1 = SelectStatement().from_(Name("efg")).select("bar")
#         stmt = SelectStatement().from_(subquery0).from_(subquery1).select(subquery0.foo, subquery1.bar)
#
#       assert
#             'SELECT "sq0"."foo","sq1"."bar" ' 'FROM (SELECT "foo" FROM "abc") "sq0",' '(SELECT "bar" FROM "efg") "sq1"',
#             str(q),
#         )
#
#     def test_select__nested_subquery(visitors):
#         subquery0 = SelectStatement().from_(Name("abc"))
#         subquery1 = SelectStatement().from_(subquery0).select(subquery0.foo, subquery0.bar)
#         subquery2 = SelectStatement().from_(subquery1).select(subquery1.foo)
#
#         stmt = SelectStatement().from_(subquery2).select(subquery2.foo)
#
#       assert
#             'SELECT "sq2"."foo" '
#             'FROM (SELECT "sq1"."foo" '
#             'FROM (SELECT "sq0"."foo","sq0"."bar" '
#             'FROM (SELECT * FROM "abc") "sq0") "sq1") "sq2"',
#             str(q),
#         )
#
def test_select__no_table(visitors):
    stmt = SelectStatement().select(1, 2, 3)
    assert visitors.mysql.sql(stmt) == "SELECT 1, 2, 3"
    assert visitors.sqlite.sql(stmt) == "SELECT 1, 2, 3"
    assert visitors.pg.sql(stmt) == "SELECT 1, 2, 3"


def test_select_then_add_table(visitors):
    stmt = SelectStatement().select(1, 2, 3).from_(Name("abc")).select("foo").select(Name("bar"))
    assert visitors.mysql.sql(stmt) == 'SELECT 1, 2, 3, \'foo\', `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"'


def test_select_with_limit(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).limit(10)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` LIMIT 10'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" LIMIT 10'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" LIMIT 10'


def test_select_with_limit_zero(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).limit(0)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc"'


def test_select_with_offset(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).offset(10)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` OFFSET 10'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" OFFSET 10'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" OFFSET 10'


def test_select_with_limit_and_offset(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).offset(10).limit(10)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` LIMIT 10 OFFSET 10'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10'


def test_select_with_force_index(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).force_index(Name("egg"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` FORCE INDEX (`egg`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg")'


def test_select_with_force_index_multiple_indexes(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).force_index(Name("egg"), Name("bacon"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `bacon`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")'


def test_select_with_force_index_multiple_calls(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).force_index(Name("egg")).force_index(Name("spam"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `spam`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")'


def test_select_with_use_index(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).use_index(Name("egg"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` USE INDEX (`egg`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg")'


def test_select_with_use_index_multiple_indexes(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).use_index(Name("egg"), Name("bacon"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` USE INDEX (`egg`, `bacon`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")'


def test_select_with_use_index_multiple_calls(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).use_index(Name("egg")).use_index(Name("spam"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` USE INDEX (`egg`, `spam`)'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")'


def test_table_select_alias(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(1)
    assert visitors.mysql.sql(stmt) == 'SELECT 1 FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT 1 FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT 1 FROM "abc"'


def test_where_basic(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo="foo")
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` = 'foo'"
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'foo\''
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'foo\''

    stmt = SelectStatement().from_(Name("abc")).where(foo=0)
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` = 0"
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = 0'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = 0'

    stmt = SelectStatement().from_(Name("abc")).where(foo=True)
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` = 1"
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = 1'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = 1'

    stmt = SelectStatement().from_(Name("abc")).where(foo=date(2020, 2, 2))
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` = '2020-02-02'"
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\''
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\''

    stmt = SelectStatement().from_(Name("abc")).where(foo=None)
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` IS NULL"
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" IS NULL'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" IS NULL'


def test_where_field_equals_for_update(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo=date(2020, 2, 2)).for_update()
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE'


def test_where_field_equals_for_update_share(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo=date(2020, 2, 2)).for_update(share=True)
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SHARE'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE'


def test_where_field_equals_for_update_nowait(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo=date(2020, 2, 2)).for_update(nowait=True)
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE NOWAIT'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT'


def test_where_field_equals_for_update_skip(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo=date(2020, 2, 2)).for_update(skip=True)
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SKIP LOCKED'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED'


def test_where_field_equals_for_update_of(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo="bar").for_update(of=("abc",))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'bar\' FOR UPDATE OF `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc"'


def test_where_field_equals_for_update_skip_locked_and_of(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo="bar").for_update(skip=True, of=("abc",))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `foo` = \'bar\' FOR UPDATE OF `abc` SKIP LOCKED'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc" SKIP LOCKED'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" = \'bar\' FOR UPDATE OF "abc" SKIP LOCKED'


def test_where_field_equals_for_multiple_tables(visitors):
    stmt = (SelectStatement().from_(Name("abc"))
            .join(Name("efg")).on(abc__id=Name("id", "efg"))
            .where(abc__foo=Name("bar", "efg"))
            )
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` JOIN `efg` ON `abc`.`id` = `efg`.`id` WHERE `abc`.`foo` = `efg`.`bar`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" JOIN "efg" ON "abc"."id" = "efg"."id" WHERE "abc"."foo" = "efg"."bar"'


def test_where_field_equals_where(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(abc__foo=1, abc__bar=Name('baz', Name("abc").name))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE `abc`.`foo` = 1 AND `abc`.`bar` = `abc`.`baz`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"'


def test_where_field_equals_where_not(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(~Binary.parse(foo=1)).where(bar=Name('baz', schema_name=Name("abc").name))
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `abc` WHERE NOT `foo` = 1 AND `bar` = `abc`.`baz`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"'


def test_where_single_quote(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo="bar'foo")
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` = 'bar''foo'"
    assert visitors.sqlite.sql(stmt) == "SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'"
    assert visitors.pg.sql(stmt) == "SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'"


def test_where_field_matches_regex(visitors):
    stmt = SelectStatement().from_(Name("abc")).where(foo__regex="r^b")
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc` WHERE `foo` REGEX 'r^b'"
    assert visitors.sqlite.sql(stmt) == "SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'"
    assert visitors.pg.sql(stmt) == "SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'"


def test_ignore_empty_criterion_where(visitors):
    stmt = SelectStatement().from_(Name("abc")).where()
    assert visitors.mysql.sql(stmt) == "SELECT * FROM `abc`"
    assert visitors.sqlite.sql(stmt) == "SELECT * FROM \"abc\""
    assert visitors.pg.sql(stmt) == "SELECT * FROM \"abc\""


def test_select_with_force_index_and_where(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo")).where(foo="bar").force_index(Name("egg"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` FORCE INDEX (`egg`) WHERE `foo` = \'bar\''
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\''
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo" = \'bar\''


def test_group_by__single(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo).select(foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" GROUP BY "foo"'


def test_group_by__multi(visitors):
    foo, bar = Name("foo"), Name("bar")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo, bar).select(foo, bar)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, `bar` FROM `abc` GROUP BY `foo`, `bar`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", "bar" FROM "abc" GROUP BY "foo", "bar"'


def test_group_by__count_star(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo).select(foo, Count(STAR))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, COUNT(*) FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", COUNT(*) FROM "abc" GROUP BY "foo"'


def test_group_by__count_field(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo).select(foo, Count(Name("bar")))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, COUNT(`bar`) FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", COUNT("bar") FROM "abc" GROUP BY "foo"'


def test_group_by__count_distinct(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo).select(foo, Count(STAR).distinct())
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, COUNT(DISTINCT *) FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"'


def test_group_by__sum_distinct(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).group_by(foo).select(foo, Sum(Name("bar")).distinct())
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, SUM(DISTINCT `bar`) FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"'


def test_group_by__alias(visitors):
    bar = Name("bar").as_("bar01")
    stmt = SelectStatement().from_(Name("abc")).select(Sum(Name("foo")), bar).group_by(bar)
    assert visitors.mysql.sql(stmt) == 'SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` GROUP BY `bar01`'
    assert visitors.sqlite.sql(stmt) == 'SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"'
    assert visitors.pg.sql(stmt) == 'SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" GROUP BY "bar01"'


def test_group_by__alias_with_join(visitors):
    table1 = Name("table1").as_("t1")
    bar = Name("bar", schema_name=table1.alias).as_("bar01")
    stmt = (SelectStatement().from_(Name("abc")).join(table1)
            .on(abc__id=Name("t_ref", schema_name=table1.alias))
            .select(Sum(Name("foo")), bar).group_by(bar))
    assert visitors.mysql.sql(stmt) == 'SELECT SUM(`foo`), `t1`.`bar` AS `bar01` FROM `abc` JOIN `table1` AS `t1` ON `abc`.`id` = `t1`.`t_ref` GROUP BY `bar01`'
    assert visitors.sqlite.sql(
        stmt) == 'SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"'
    assert visitors.pg.sql(stmt) == 'SELECT SUM("foo"), "t1"."bar" AS "bar01" FROM "abc" JOIN "table1" AS "t1" ON "abc"."id" = "t1"."t_ref" GROUP BY "bar01"'


def test_mysql_query_uses_backtick_quote_chars(visitors):
    stmt = SelectStatement().from_(Name("abc")).group_by(Name('foo')).select(Name('foo'))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` GROUP BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" GROUP BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" GROUP BY "foo"'


def test_having_greater_than(visitors):
    foo, bar = Name('foo'), Name('bar')
    stmt = SelectStatement().from_(Name("abc")).select(foo, Sum(bar)).group_by(foo).having(Sum(bar).gt(1))

    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING SUM(`bar`) > 1'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1'


def test_having_and(visitors):
    foo, bar = Name('foo'), Name('bar')
    stmt = SelectStatement().from_(Name("abc")).select(foo, Sum(bar)).group_by(foo).having((Sum(bar).gt(1)) & (Sum(bar).lt(100)))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, SUM(`bar`) FROM `abc` GROUP BY `foo` HAVING SUM(`bar`) > 1 AND SUM(`bar`) < 100'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1 AND SUM("bar") < 100'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar") > 1 AND SUM("bar") < 100'


def test_having_join_and_equality(visitors):
    abc_foo = Name('foo', schema_name=Name("abc").name)
    abc_buz = Name('buz', schema_name=Name("abc").name)
    efg_foo = Name('foo', schema_name=Name("efg").name)
    efg_bar = Name('bar', schema_name=Name("efg").name)

    stmt = (
        SelectStatement().from_(Name("abc")).join(Name("efg"))
        .on(abc__foo=efg_foo)
        .select(abc_foo, Sum(efg_bar), abc_buz)
        .group_by(abc_foo)
        .having(abc__buz="fiz")
        .having(Sum(efg_bar).gt(100))
    )

    assert visitors.mysql.sql(stmt) == ('SELECT `abc`.`foo`, SUM(`efg`.`bar`), `abc`.`buz` FROM `abc` '
                                        'JOIN `efg` ON `abc`.`foo` = `efg`.`foo` GROUP BY `abc`.`foo` '
                                        'HAVING `abc`.`buz` = \'fiz\' AND SUM(`efg`.`bar`) > 100')
    assert visitors.sqlite.sql(stmt) == ('SELECT "abc"."foo", SUM("efg"."bar"), "abc"."buz" FROM "abc" '
                                         'JOIN "efg" ON "abc"."foo" = "efg"."foo" GROUP BY "abc"."foo" '
                                         'HAVING "abc"."buz" = \'fiz\' AND SUM("efg"."bar") > 100')
    assert visitors.pg.sql(stmt) == ('SELECT "abc"."foo", SUM("efg"."bar"), "abc"."buz" FROM "abc" '
                                     'JOIN "efg" ON "abc"."foo" = "efg"."foo" GROUP BY "abc"."foo" '
                                     'HAVING "abc"."buz" = \'fiz\' AND SUM("efg"."bar") > 100')


def test_order_by__single_field(visitors):
    stmt = SelectStatement().from_(Name("abc")).order_by(Name("foo")).select(Name("foo"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` ORDER BY `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo"'


def test_order_by__multi_fields(visitors):
    foo, bar = Name("foo"), Name("bar")
    stmt = SelectStatement().from_(Name("abc")).order_by(foo, bar).select(foo, bar)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo`, `bar` FROM `abc` ORDER BY `foo`, `bar`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo", "bar" FROM "abc" ORDER BY "foo", "bar"'


def test_order_by_asc(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).order_by(foo, sorted_in=SortedIn.ASC).select(foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` ORDER BY `foo` ASC'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo" ASC'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo" ASC'


def test_order_by_desc(visitors):
    foo = Name("foo")
    stmt = SelectStatement().from_(Name("abc")).order_by(foo, sorted_in=SortedIn.DESC).select(foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` FROM `abc` ORDER BY `foo` DESC'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo" DESC'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" FROM "abc" ORDER BY "foo" DESC'


def test_order_by__alias(visitors):
    bar = Name("bar").as_("bar01")
    stmt = SelectStatement().from_(Name("abc")).select(Sum(Name("foo")), bar).order_by(bar)
    assert visitors.mysql.sql(stmt) == 'SELECT SUM(`foo`), `bar` AS `bar01` FROM `abc` ORDER BY `bar01`'
    assert visitors.sqlite.sql(stmt) == 'SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"'
    assert visitors.pg.sql(stmt) == 'SELECT SUM("foo"), "bar" AS "bar01" FROM "abc" ORDER BY "bar01"'


def test_table_field(visitors):
    bar = Name("foo").as_("bar")
    stmt = SelectStatement().from_(Name("abc")).select(bar)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc"'


def test_table_field__multi(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Name("foo").as_("bar"), Name("fiz").as_("buz"))
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar`, `fiz` AS `buz` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar", "fiz" AS "buz" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar", "fiz" AS "buz" FROM "abc"'


# def test_arithmetic_function(visitors):
#     """ @todo: support arithmetic """
#     stmt = SelectStatement().from_(Name("abc")).select((self.t.foo + self.t.bar).as_("biz"))
# visitors.mysql.sql(stmt) ==     self.assertEqual('SELECT "foo"+"bar" "biz" FROM "abc"'

def test_alias_functions(visitors):
    stmt = SelectStatement().from_(Name("abc")).select(Count(STAR).as_("foo"))
    assert visitors.mysql.sql(stmt) == 'SELECT COUNT(*) AS `foo` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT COUNT(*) AS "foo" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT COUNT(*) AS "foo" FROM "abc"'


def test_alias_function_using_as_nested(visitors):
    """ We don't show aliases of fields that are arguments of a function. """
    stmt = SelectStatement().from_(Name("abc")).select(Sqrt(Count(STAR).as_("foo")).as_("bar"))
    assert visitors.mysql.sql(stmt) == 'SELECT SQRT(COUNT(*)) AS `bar` FROM `abc`'
    assert visitors.sqlite.sql(stmt) == 'SELECT SQRT(COUNT(*)) AS "bar" FROM "abc"'
    assert visitors.pg.sql(stmt) == 'SELECT SQRT(COUNT(*)) AS "bar" FROM "abc"'


def test_alias_in__group_by(visitors):
    foo = Name('foo').as_('bar')
    stmt = SelectStatement().from_(Name("abc")).select(foo).group_by(foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc` GROUP BY `bar`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" GROUP BY "bar"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" GROUP BY "bar"'


def test_alias_in__order_by(visitors):
    foo = Name('foo').as_('bar')
    stmt = SelectStatement().from_(Name("abc")).select(foo).order_by(foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc` ORDER BY `bar`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" ORDER BY "bar"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" ORDER BY "bar"'


def test_alias_ignored__in_value(visitors):
    foo = Name('foo').as_('bar')
    stmt = SelectStatement().from_(Name("abc")).select(foo).where(username=foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `foo` AS `bar` FROM `abc` WHERE `username` = `foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" WHERE "username" = "foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "foo" AS "bar" FROM "abc" WHERE "username" = "foo"'


def test_select__multiple_tables(visitors):
    table_abc = Name("abc").as_("t0")
    table_efg = Name("efg").as_("t1")
    foo = Name('foo', schema_name=table_abc)
    bar = Name('bar', schema_name=table_efg)
    stmt = SelectStatement().from_(table_abc).select(foo).from_(table_efg).select(bar)
    assert visitors.mysql.sql(stmt) == 'SELECT `t0`.`foo`, `t1`.`bar` FROM `abc` AS `t0`, `efg` AS `t1`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "t0"."foo", "t1"."bar" FROM "abc" AS "t0", "efg" AS "t1"'
    assert visitors.pg.sql(stmt) == 'SELECT "t0"."foo", "t1"."bar" FROM "abc" AS "t0", "efg" AS "t1"'


def test_use_aliases_in__group_by_and_order_by(visitors):
    table_abc = Name("abc").as_("t0")
    my_foo = Name("foo", table_abc.alias).as_("my_foo")
    bar = Name("bar", table_abc.alias)
    stmt = SelectStatement().from_(table_abc).select(my_foo, bar).group_by(my_foo).order_by(my_foo)
    assert visitors.mysql.sql(stmt) == 'SELECT `t0`.`foo` AS `my_foo`, `t0`.`bar` FROM `abc` AS `t0` GROUP BY `my_foo` ORDER BY `my_foo`'
    assert visitors.sqlite.sql(stmt) == 'SELECT "t0"."foo" AS "my_foo", "t0"."bar" FROM "abc" AS "t0" GROUP BY "my_foo" ORDER BY "my_foo"'
    assert visitors.pg.sql(stmt) == 'SELECT "t0"."foo" AS "my_foo", "t0"."bar" FROM "abc" AS "t0" GROUP BY "my_foo" ORDER BY "my_foo"'


def test_table_with_schema_and_alias(visitors):
    table = Name("abc", schema_name="schema").as_("alias")
    stmt = SelectStatement().from_(table)
    assert visitors.mysql.sql(stmt) == 'SELECT * FROM `schema`.`abc` AS `alias`'
    assert visitors.sqlite.sql(stmt) == 'SELECT * FROM "schema"."abc" AS "alias"'
    assert visitors.pg.sql(stmt) == 'SELECT * FROM "schema"."abc" AS "alias"'


def test_extraneous_quotes(visitors):
    t1 = Name("table1").as_("t1")
    t2 = Name("table2").as_("t2")
    stmt = SelectStatement().from_(t1).join(t2).on(t1__value__bt=(Name("start", schema_name=t2), Name("end", schema_name=t2))).select(
        Name("value", schema_name=t1))
    assert visitors.mysql.sql(stmt) == 'SELECT `t1`.`value` FROM `table1` AS `t1` JOIN `table2` AS `t2` ON `t1`.`value` BETWEEN `t2`.`start` AND `t2`.`end`'
    assert visitors.sqlite.sql(
        stmt) == 'SELECT "t1"."value" FROM "table1" AS "t1" JOIN "table2" AS "t2" ON "t1"."value" BETWEEN "t2"."start" AND "t2"."end"'
    assert visitors.pg.sql(stmt) == 'SELECT "t1"."value" FROM "table1" AS "t1" JOIN "table2" AS "t2" ON "t1"."value" BETWEEN "t2"."start" AND "t2"."end"'

    # class SubqueryTests(unittest.TestCase):
    #     maxDiff = None
    #
    #     table_abc, table_efg, table_hij = Tables("abc", "efg", "hij")
    #
    #     def test_where__in(visitors):
    #         stmt = (
    #             SelectStatement().from_(Name("abc"))
    #
    #             .where(
    #                 Name("abc").foo.isin(
    #                     SelectStatement().from_(Name("efg")).select(Name("efg").foo).where(Name("efg").bar == 0)
    #                 )
    #             )
    #         )
    #
    #       assert
    #             'SELECT * FROM "abc" WHERE "foo" IN (SELECT "foo" FROM "efg" WHERE "bar"=0)',
    #             str(q),
    #         )
    #
    #     def test_where__in_nested(visitors):
    #         stmt = SelectStatement().from_(Name("abc")).where(Name("abc").foo).isin(Name("efg"))
    #         assert visitors.mysql.sql(stmt) == 'SELECT * FROM "abc" WHERE "foo" IN (SELECT * FROM "efg")'
    #
    #     def test_join(visitors):
    #         subquery = SelectStatement().from_("efg").select("fiz", "buz").where(F("buz") == 0)
    #
    #         stmt = (
    #             SelectStatement().from_(Name("abc"))
    #             .join(subquery)
    #             .on(Name("abc").bar == subquery.buz)
    #             .select(Name("abc").foo, subquery.fiz)
    #         )
    #
    #       assert
    #             'SELECT "abc"."foo","sq0"."fiz" FROM "abc" '
    #             'JOIN (SELECT "fiz","buz" FROM "efg" WHERE "buz"=0) "sq0" '
    #             'ON "abc"."bar"="sq0"."buz"',
    #             str(q),
    #         )
    #
    #     def test_select_subquery(visitors):
    #         substmt = SelectStatement().from_(Name("efg")).select("fizzbuzz").where(Name("efg").id == 1)
    #
    #         stmt = SelectStatement().from_(Name("abc")).select("foo", "bar").select(subq)
    #
    #       assert
    #             'SELECT "foo","bar",(SELECT "fizzbuzz" FROM "efg" WHERE "id"=1) ' 'FROM "abc"',
    #             str(q),
    #         )
    #
    #     def test_select_subquery_with_alias(visitors):
    #         substmt = SelectStatement().from_(Name("efg")).select("fizzbuzz").where(Name("efg").id == 1)
    #
    #         stmt = SelectStatement().from_(Name("abc")).select("foo", "bar").select(subq.as_("sq"))
    #
    #       assert
    #             'SELECT "foo","bar",(SELECT "fizzbuzz" FROM "efg" WHERE "id"=1) "sq" ' 'FROM "abc"',
    #             str(q),
    #         )
    #
    #     def test_where__equality(visitors):
    #         subquery = SelectStatement().from_("efg").select("fiz").where(F("buz") == 0)
    #         query = (
    #             SelectStatement().from_(Name("abc"))
    #             .select(Name("abc").foo, Name("abc").bar)
    #             .where(Name("abc").bar == subquery)
    #         )
    #
    #       assert
    #             'SELECT "foo","bar" FROM "abc" ' 'WHERE "bar"=(SELECT "fiz" FROM "efg" WHERE "buz"=0)',
    #             str(query),
    #         )
    #
    #     def test_select_from_nested_query(visitors):
    #         subquery = SelectStatement().from_(Name("abc")).select(
    #             Name("abc").foo,
    #             Name("abc").bar,
    #             (Name("abc").fizz + Name("abc").buzz).as_("fizzbuzz"),
    #         )
    #
    #         query = SelectStatement().from_(subquery).select(subquery.foo, subquery.bar, subquery.fizzbuzz)
    #
    #       assert
    #             'SELECT "sq0"."foo","sq0"."bar","sq0"."fizzbuzz" '
    #             "FROM ("
    #             'SELECT "foo","bar","fizz"+"buzz" "fizzbuzz" '
    #             'FROM "abc"'
    #             ') "sq0"',
    #             str(query),
    #         )
    #
    #     def test_select_from_nested_query_with_join(visitors):
    #         subquery1 = (
    #             SelectStatement().from_(Name("abc"))
    #             .select(
    #                 Name("abc").foo,
    #                 fn.Sum(Name("abc").fizz + Name("abc").buzz).as_("fizzbuzz"),
    #             )
    #             .group_by(Name("abc").foo)
    #         )
    #
    #         subquery2 = SelectStatement().from_(Name("efg")).select(
    #             Name("efg").foo.as_("foo_two"),
    #             Name("efg").bar,
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
    #       assert
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
    #     def test_from_subquery_without_alias(visitors):
    #         subquery = SelectStatement().from_(Name("efg")).select(
    #             Name("efg").base_id.as_("x"), Name("efg").fizz, Name("efg").buzz
    #         )
    #
    #         test_query = SelectStatement().from_(subquery).select(subquery.x, subquery.fizz, subquery.buzz)
    #
    #       assert
    #             'SELECT "sq0"."x","sq0"."fizz","sq0"."buzz" '
    #             "FROM ("
    #             'SELECT "base_id" "x","fizz","buzz" FROM "efg"'
    #             ') "sq0"',
    #             str(test_query),
    #         )
    #
    #     def test_join_query_with_alias(visitors):
    #         subquery = (
    #             SelectStatement().from_(Name("efg"))
    #             .select(
    #                 Name("efg").base_id.as_("x"),
    #                 Name("efg").fizz,
    #                 Name("efg").buzz,
    #             )
    #             .as_("subq")
    #         )
    #
    #         test_query = SelectStatement().from_(subquery).select(subquery.x, subquery.fizz, subquery.buzz)
    #
    #       assert
    #             'SELECT "subq"."x","subq"."fizz","subq"."buzz" '
    #             "FROM ("
    #             'SELECT "base_id" "x","fizz","buzz" FROM "efg"'
    #             ') "subq"',
    #             str(test_query),
    #         )
    #
    #     def test_with(visitors):
    #         sub_query = SelectStatement().from_(Name("efg")).select("fizz")
    #         test_query = SelectStatement().with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
    #
    #       assert
    #             'WITH an_alias AS (SELECT "fizz" FROM "efg") SELECT * FROM an_alias',
    #             str(test_query),
    #         )
    #
    #     def test_join_with_with(visitors):
    #         sub_query = SelectStatement().from_(Name("efg")).select("fizz")
    #         test_query = (
    #             SelectStatement().with_(sub_query, "an_alias")
    #             .from_(Name("abc"))
    #             .join(AliasedQuery("an_alias"))
    #             .on(AliasedQuery("an_alias").fizz == Name("abc").buzz)
    #
    #         )
    #       assert
    #             'WITH an_alias AS (SELECT "fizz" FROM "efg") '
    #             'SELECT * FROM "abc" JOIN an_alias ON "an_alias"."fizz"="abc"."buzz"',
    #             str(test_query),
    #         )
    #
    #     def test_select_from_with_returning(visitors):
    #         sub_query = SelectStatement().into(Name("abc")).insert(1).returning('*')
    #         test_query = SelectStatement().with_(sub_query, "an_alias").from_(AliasedQuery("an_alias"))
    #       assert
    #             'WITH an_alias AS (INSERT INTO "abc" VALUES (1) RETURNING *) SELECT * FROM an_alias', str(test_query)
    #         )
    #
    #


if __name__ == '__main__':
    unittest.main()
