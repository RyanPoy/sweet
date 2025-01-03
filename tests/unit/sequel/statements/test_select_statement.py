import unittest
from datetime import date

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms.name import ColumnName, IndexName, TableName
from sweet.sequel.terms.q import Q
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestSelectStatement(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()
        self.table_abc = TableName("abc")
        self.table_efg = TableName("efg")

    def test_empty_query(self):
        stmt = SelectStatement().from_(self.table_abc)
        self.assertEqual('SELECT * FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc"', stmt.sql(self.pg))

    def test_select__table_schema(self):
        stmt = SelectStatement().from_(TableName("abc", "schema1"))
        self.assertEqual('SELECT * FROM `schema1`.`abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "schema1"."abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "schema1"."abc"', stmt.sql(self.pg))

    def test_select__distinct__single(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).distinct()
        self.assertEqual('SELECT DISTINCT `foo` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT DISTINCT "foo" FROM "abc"', stmt.sql(self.pg))

    def test_select__distinct__multi(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo"), ColumnName("bar")).distinct()
        self.assertEqual('SELECT DISTINCT `foo`, `bar` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT DISTINCT "foo", "bar" FROM "abc"', stmt.sql(self.pg))

    def test_select_single_column(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo"))
        self.assertEqual('SELECT `foo` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc"', stmt.sql(self.pg))

    def test_select_single_column_with_alias(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc"', stmt.sql(self.pg))

    def test_select_single_column_and_table_alias_str(self):
        stmt = SelectStatement().from_(self.table_abc.as_("fizzbuzz")).select(ColumnName("foo").as_("bar"))
        self.assertEqual('SELECT `foo` AS `bar` FROM `abc` AS `fizzbuzz`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" AS "bar" FROM "abc" AS "fizzbuzz"', stmt.sql(self.pg))

    def test_select_multiple_columns(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).select(ColumnName("bar"))
        self.assertEqual('SELECT `foo`, `bar` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo", "bar" FROM "abc"', stmt.sql(self.pg))

    def test_select_multiple_tables(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo", self.table_abc.value)) \
            .from_(self.table_efg).select(ColumnName("bar", self.table_efg.value))
        self.assertEqual('SELECT `abc`.`foo`, `efg`.`bar` FROM `abc`, `efg`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "abc"."foo", "efg"."bar" FROM "abc", "efg"', stmt.sql(self.pg))

    # def test_select_subquery(self):
    #     sub = SelectStatement().from_(self.table_abc)
    #     stmt = SelectStatement().from_(sub).select(ColumnName("foo"), ColumnName("bar"))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', stmt.sql(self.mysql))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', stmt.sql(self.sqlite))
    #     self.assertEqual('SELECT "sq0"."foo", "sq0"."bar" FROM (SELECT * FROM "abc") AS "sq0"', stmt.sql(self.pg))

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
        self.assertEqual("SELECT 1, 2, 3", stmt.sql(self.mysql))
        self.assertEqual("SELECT 1, 2, 3", stmt.sql(self.sqlite))
        self.assertEqual("SELECT 1, 2, 3", stmt.sql(self.pg))

    def test_select_then_add_table(self):
        stmt = SelectStatement().select(1, 2, 3).from_(self.table_abc).select("foo").select(ColumnName("bar"))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', `bar` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT 1, 2, 3, \'foo\', "bar" FROM "abc"', stmt.sql(self.pg))

    def test_select_with_limit(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10', stmt.sql(self.pg))

    def test_select_with_limit_zero(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).limit(0)
        self.assertEqual('SELECT `foo` FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc"', stmt.sql(self.pg))

    def test_select_with_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).offset(10)
        self.assertEqual('SELECT `foo` FROM `abc` OFFSET 10', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" OFFSET 10', stmt.sql(self.pg))

    def test_select_with_limit_and_offset(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).offset(10).limit(10)
        self.assertEqual('SELECT `foo` FROM `abc` LIMIT 10 OFFSET 10', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" LIMIT 10 OFFSET 10', stmt.sql(self.pg))

    def test_select_with_force_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(IndexName("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg")', stmt.sql(self.pg))

    def test_select_with_force_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(IndexName("egg"), IndexName("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `bacon`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "bacon")', stmt.sql(self.pg))

    def test_select_with_force_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).force_index(IndexName("egg")).force_index(IndexName("spam"))
        self.assertEqual('SELECT `foo` FROM `abc` FORCE INDEX (`egg`, `spam`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg", "spam")', stmt.sql(self.pg))

    def test_select_with_use_index(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(IndexName("egg"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg")', stmt.sql(self.pg))

    def test_select_with_use_index_multiple_indexes(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(IndexName("egg"), IndexName("bacon"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`, `bacon`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "bacon")', stmt.sql(self.pg))

    def test_select_with_use_index_multiple_calls(self):
        stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).use_index(IndexName("egg")).use_index(IndexName("spam"))
        self.assertEqual('SELECT `foo` FROM `abc` USE INDEX (`egg`, `spam`)', stmt.sql(self.mysql))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")', stmt.sql(self.sqlite))
        self.assertEqual('SELECT "foo" FROM "abc" USE INDEX ("egg", "spam")', stmt.sql(self.pg))

    def test_table_select_alias(self):
        stmt = SelectStatement().from_(self.table_abc).select(1)
        self.assertEqual('SELECT 1 FROM `abc`', stmt.sql(self.mysql))
        self.assertEqual('SELECT 1 FROM "abc"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT 1 FROM "abc"', stmt.sql(self.pg))

    def test_where_basic(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="foo")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 'foo'", stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'foo\'', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'foo\'', stmt.sql(self.pg))

        stmt = SelectStatement().from_(self.table_abc).where(foo=0)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 0", stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 0', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 0', stmt.sql(self.pg))

        stmt = SelectStatement().from_(self.table_abc).where(foo=True)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 1", stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 1', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = 1', stmt.sql(self.pg))

        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2))
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = '2020-02-02'", stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\'', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\'', stmt.sql(self.pg))

        stmt = SelectStatement().from_(self.table_abc).where(foo=None)
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` IS NULL", stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" IS NULL', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" IS NULL', stmt.sql(self.pg))

    def test_where_field_equals_for_update(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update()
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE', stmt.sql(self.pg))

    def test_where_field_equals_for_update_share(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(share=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SHARE', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SHARE', stmt.sql(self.pg))

    def test_where_field_equals_for_update_nowait(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(nowait=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE NOWAIT', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE NOWAIT', stmt.sql(self.pg))

    def test_where_field_equals_for_update_skip(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo=date(2020, 2, 2)).for_update(skip=True)
        self.assertEqual('SELECT * FROM `abc` WHERE `foo` = \'2020-02-02\' FOR UPDATE SKIP LOCKED', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "foo" = \'2020-02-02\' FOR UPDATE SKIP LOCKED', stmt.sql(self.pg))

    def test_where_field_equals_for_multiple_tables(self):
        stmt = (SelectStatement().from_(self.table_abc)
                .join(self.table_efg).on(abc__id=ColumnName("id", "efg"))
                .where(abc__foo=ColumnName("bar", "efg"))
                )
        self.assertEqual('SELECT * FROM `abc` JOIN `efg` ON `abc`.`id` = `efg`.`id` WHERE `abc`.`foo` = `efg`.`bar`', stmt.sql(self.mysql))

    def test_where_field_equals_where(self):
        stmt = SelectStatement().from_(self.table_abc).where(abc__foo=1).where(abc__bar=self.table_abc.baz)
        self.assertEqual('SELECT * FROM `abc` WHERE `abc`.`foo` = 1 AND `abc`.`bar` = `abc`.`baz`', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE "abc"."foo" = 1 AND "abc"."bar" = "abc"."baz"', stmt.sql(self.pg))

    def test_where_field_equals_where_not(self):
        stmt = SelectStatement().from_(self.table_abc).where(~Q(foo=1)).where(bar=self.table_abc.baz)
        self.assertEqual('SELECT * FROM `abc` WHERE NOT `foo` = 1 AND `bar` = `abc`.`baz`', stmt.sql(self.mysql))
        self.assertEqual('SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"', stmt.sql(self.sqlite))
        self.assertEqual('SELECT * FROM "abc" WHERE NOT "foo" = 1 AND "bar" = "abc"."baz"', stmt.sql(self.pg))

    def test_where_single_quote(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo="bar'foo")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` = 'bar''foo'", stmt.sql(self.mysql))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'", stmt.sql(self.sqlite))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" = 'bar''foo'", stmt.sql(self.pg))

    def test_where_field_matches_regex(self):
        stmt = SelectStatement().from_(self.table_abc).where(foo__regex="r^b")
        self.assertEqual("SELECT * FROM `abc` WHERE `foo` REGEX 'r^b'", stmt.sql(self.mysql))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'", stmt.sql(self.sqlite))
        self.assertEqual("SELECT * FROM \"abc\" WHERE \"foo\" REGEX 'r^b'", stmt.sql(self.pg))

    def test_ignore_empty_criterion_where(self):
        stmt = SelectStatement().from_(self.table_abc).where(Q())
        self.assertEqual("SELECT * FROM `abc`", stmt.sql(self.mysql))
        self.assertEqual("SELECT * FROM \"abc\"", stmt.sql(self.sqlite))
        self.assertEqual("SELECT * FROM \"abc\"", stmt.sql(self.pg))
    #
    # def test_ignore_empty_criterion_having(self):
    #     stmt = SelectStatement().from_(self.t).having(EmptyCriterion())
    #
    #     self.assertEqual('SELECT * FROM "abc"', str(q1))
    #
    # def test_select_with_force_index_and_where(self):
    #     stmt = SelectStatement().from_(self.table_abc).select(ColumnName("foo")).where(self.t.foo == self.t.bar).force_index("egg")
    #
    #     self.assertEqual('SELECT "foo" FROM "abc" FORCE INDEX ("egg") WHERE "foo"="bar"', stmt.sql(self.mysql))
    #
    # def test_where_with_multiple_wheres_using_and_case(self):
    #     case_stmt = Case().when(self.t.foo == 'bar', 1).else_(0)
    #     query = SelectStatement().from_(self.t).select(case_stmt).where(case_stmt & self.t.blah.isin(['test']))
    #
    #     self.assertEqual(
    #         'SELECT CASE WHEN "foo"=\'bar\' THEN 1 ELSE 0 END FROM "abc" WHERE CASE WHEN "foo"=\'bar\' THEN 1 ELSE 0 '
    #         'END AND "blah" IN (\'test\')',
    #         str(query),
    #     )
    #
    # def test_where_with_multiple_wheres_using_or_case(self):
    #     case_stmt = Case().when(self.t.foo == 'bar', 1).else_(0)
    #     query = SelectStatement().from_(self.t).select(case_stmt).where(case_stmt | self.t.blah.isin(['test']))
    #
    #     self.assertEqual(
    #         'SELECT CASE WHEN "foo"=\'bar\' THEN 1 ELSE 0 END FROM "abc" WHERE CASE WHEN "foo"=\'bar\' THEN 1 ELSE 0 '
    #         'END OR "blah" IN (\'test\')',
    #         str(query),
    #     )

#
# class PreWhereTests(WhereTests):
#     t = Table("abc")
#
#     def test_prewhere_field_equals(self):
#         stmt = SelectStatement().from_(self.t).prewhere(self.t.foo == self.t.bar)
#         q2 = SelectStatement().from_(self.t).prewhere(self.t.foo.eq(self.t.bar))
#
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar"', str(q1))
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar"', str(q2))
#
#     def test_where_and_prewhere(self):
#         stmt = SelectStatement().from_(self.t).prewhere(self.t.foo == self.t.bar).where(self.t.foo == self.t.bar)
#
#         self.assertEqual('SELECT * FROM "abc" PREWHERE "foo"="bar" WHERE "foo"="bar"', stmt.sql(self.mysql))
#
#

# class WhereTests(unittest.TestCase):
#
#     def test_where_field_equals_for_update_of(self):
#         for query_cls in [
#             MySQLQuery,
#             PostgreSQLQuery,
#         ]:
#             quote_char = query_cls._builder().QUOTE_CHAR if isinstance(query_cls._builder().QUOTE_CHAR, str) else '"'
#             stmt = query_cls.from_(self.t).where(self.t.foo == self.t.bar).for_update(of=("abc",))
#             self.assertEqual(
#                 'SELECT * '
#                 'FROM {quote_char}abc{quote_char} '
#                 'WHERE {quote_char}foo{quote_char}={quote_char}bar{quote_char} '
#                 'FOR UPDATE OF {quote_char}abc{quote_char}'.format(
#                     quote_char=quote_char,
#                 ),
#                 str(q),
#             )
#     def test_where_field_equals_for_update_skip_locked_and_of(self):
#         for query_cls in [
#             MySQLQuery,
#             PostgreSQLQuery,
#         ]:
#             stmt = (
#                 query_cls.from_(self.t)
#
#                 .where(self.t.foo == self.t.bar)
#                 .for_update(nowait=False, skip_locked=True, of=("abc",))
#             )
#             quote_char = query_cls._builder().QUOTE_CHAR if isinstance(query_cls._builder().QUOTE_CHAR, str) else '"'
#             self.assertEqual(
#                 'SELECT * '
#                 'FROM {quote_char}abc{quote_char} '
#                 'WHERE {quote_char}foo{quote_char}={quote_char}bar{quote_char} '
#                 'FOR UPDATE OF {quote_char}abc{quote_char} SKIP LOCKED'.format(
#                     quote_char=quote_char,
#                 ),
#                 str(q),
#             )
#
#
# class GroupByTests(unittest.TestCase):
#     t = Table("abc")
#     maxDiff = None
#
#     def test_groupby__single(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__multi(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo, self.t.bar).select(self.t.foo, self.t.bar)
#
#         self.assertEqual('SELECT "foo","bar" FROM "abc" GROUP BY "foo","bar"', stmt.sql(self.mysql))
#
#     def test_groupby__count_star(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo, fn.Count("*"))
#
#         self.assertEqual('SELECT "foo",COUNT(*) FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__count_field(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo, fn.Count(self.t.bar))
#
#         self.assertEqual('SELECT "foo",COUNT("bar") FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__count_distinct(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo, fn.Count("*").distinct())
#
#         self.assertEqual('SELECT "foo",COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__sum_distinct(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo, fn.Sum(self.t.bar).distinct())
#
#         self.assertEqual('SELECT "foo",SUM(DISTINCT "bar") FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__sum_filter(self):
#         stmt = (
#             SelectStatement().from_(self.t)
#             .groupby(self.t.foo)
#             .select(self.t.foo, fn.Sum(self.t.bar).filter(self.t.id.eq(1) & self.t.cid.gt(2)))
#         )
#
#         self.assertEqual('SELECT "foo",SUM("bar") FILTER(WHERE "id"=1 AND "cid">2) FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__str(self):
#         stmt = SelectStatement().from_(self.table_abc).groupby("foo").select("foo", fn.Count("*").distinct())
#
#         self.assertEqual('SELECT "foo",COUNT(DISTINCT *) FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_groupby__int(self):
#         stmt = SelectStatement().from_(self.table_abc).groupby(1).select("foo", fn.Count("*").distinct())
#
#         self.assertEqual('SELECT "foo",COUNT(DISTINCT *) FROM "abc" GROUP BY 1', stmt.sql(self.mysql))
#
#     def test_groupby__alias(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = SelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#         self.assertEqual('SELECT SUM("foo"),"bar" "bar01" FROM "abc" GROUP BY "bar01"', stmt.sql(self.mysql))
#
#     def test_groupby__no_alias(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = SelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#         self.assertEqual(
#             'SELECT SUM("foo"),"bar" "bar01" FROM "abc" GROUP BY "bar"',
#             q.get_sql(groupby_alias=False),
#         )
#
#     def test_groupby__no_alias_mssql(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = MSSQLSelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#         self.assertEqual('SELECT SUM("foo"),"bar" "bar01" FROM "abc" GROUP BY "bar"', stmt.sql(self.mysql))
#
#     def test_groupby__no_alias_oracle(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = OracleSelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#         self.assertEqual('SELECT SUM(foo),bar bar01 FROM abc GROUP BY bar', stmt.sql(self.mysql))
#
#     def test_groupby__alias_platforms(self):
#         bar = self.t.bar.as_("bar01")
#
#         for query_cls in [
#             MySQLQuery,
#             VerticaQuery,
#             PostgreSQLQuery,
#             RedshiftQuery,
#             ClickHouseQuery,
#             SQLLiteQuery,
#         ]:
#             stmt = query_cls.from_(self.t).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#             quote_char = query_cls._builder().QUOTE_CHAR if isinstance(query_cls._builder().QUOTE_CHAR, str) else '"'
#
#             self.assertEqual(
#                 "SELECT "
#                 "SUM({quote_char}foo{quote_char}),"
#                 "{quote_char}bar{quote_char}{as_keyword}{quote_char}bar01{quote_char} "
#                 "FROM {quote_char}abc{quote_char} "
#                 "GROUP BY {quote_char}bar01{quote_char}".format(
#                     as_keyword=' AS ' if query_cls is ClickHouseQuery else ' ', quote_char=quote_char
#                 ),
#                 str(q),
#             )
#
#     def test_groupby__alias_with_join(self):
#         table1 = Table("table1", alias="t1")
#         bar = table1.bar.as_("bar01")
#         stmt = SelectStatement().from_(self.t).join(table1).on(self.t.id == table1.t_ref).select(fn.Sum(self.t.foo), bar).groupby(bar)
#
#         self.assertEqual(
#             'SELECT SUM("abc"."foo"),"t1"."bar" "bar01" FROM "abc" '
#             'JOIN "table1" "t1" ON "abc"."id"="t1"."t_ref" '
#             'GROUP BY "bar01"',
#             str(q),
#         )
#
#     def test_groupby_with_case_uses_the_alias(self):
#         stmt = (
#             SelectStatement().from_(self.t)
#             .select(
#                 fn.Sum(self.t.foo).as_("bar"),
#                 Case().when(self.t.fname == "Tom", "It was Tom").else_("It was someone else.").as_("who_was_it"),
#             )
#             .groupby(Case().when(self.t.fname == "Tom", "It was Tom").else_("It was someone else.").as_("who_was_it"))
#         )
#
#         self.assertEqual(
#             'SELECT SUM("foo") "bar",'
#             "CASE WHEN \"fname\"='Tom' THEN 'It was Tom' "
#             "ELSE 'It was someone else.' END \"who_was_it\" "
#             'FROM "abc" '
#             'GROUP BY "who_was_it"',
#             str(q),
#         )
#
#     def test_mysql_query_uses_backtick_quote_chars(self):
#         stmt = MySQLSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual("SELECT `foo` FROM `abc` GROUP BY `foo`", stmt.sql(self.mysql))
#
#     def test_vertica_query_uses_double_quote_chars(self):
#         stmt = VerticaSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_mssql_query_uses_double_quote_chars(self):
#         stmt = MSSQLSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_oracle_query_uses_no_quote_chars(self):
#         stmt = OracleSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT foo FROM abc GROUP BY foo', stmt.sql(self.mysql))
#
#     def test_postgres_query_uses_double_quote_chars(self):
#         stmt = PostgreSQLSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_redshift_query_uses_double_quote_chars(self):
#         stmt = RedshiftSelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_group_by__single_with_totals(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo).select(self.t.foo).with_totals()
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo" WITH TOTALS', stmt.sql(self.mysql))
#
#     def test_groupby__multi_with_totals(self):
#         stmt = SelectStatement().from_(self.t).groupby(self.t.foo, self.t.bar).select(self.t.foo, self.t.bar).with_totals()
#
#         self.assertEqual('SELECT "foo","bar" FROM "abc" GROUP BY "foo","bar" WITH TOTALS', stmt.sql(self.mysql))
#
#
# class HavingTests(unittest.TestCase):
#     table_abc, table_efg = Tables("abc", "efg")
#
#     def test_having_greater_than(self):
#         stmt = (
#             SelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo, fn.Sum(self.table_abc.bar))
#             .groupby(self.table_abc.foo)
#             .having(fn.Sum(self.table_abc.bar) > 1)
#         )
#
#         self.assertEqual(
#             'SELECT "foo",SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar")>1',
#             str(q),
#         )
#
#     def test_having_and(self):
#         stmt = (
#             SelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo, fn.Sum(self.table_abc.bar))
#             .groupby(self.table_abc.foo)
#             .having((fn.Sum(self.table_abc.bar) > 1) & (fn.Sum(self.table_abc.bar) < 100))
#         )
#
#         self.assertEqual(
#             'SELECT "foo",SUM("bar") FROM "abc" GROUP BY "foo" HAVING SUM("bar")>1 AND SUM("bar")<100',
#             str(q),
#         )
#
#     def test_having_join_and_equality(self):
#         stmt = (
#             SelectStatement().from_(self.table_abc)
#             .join(self.table_efg)
#             .on(self.table_abc.foo == self.table_efg.foo)
#             .select(self.table_abc.foo, fn.Sum(self.table_efg.bar), self.table_abc.buz)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#             .having(fn.Sum(self.table_efg.bar) > 100)
#         )
#
#         self.assertEqual(
#             'SELECT "abc"."foo",SUM("efg"."bar"),"abc"."buz" FROM "abc" '
#             'JOIN "efg" ON "abc"."foo"="efg"."foo" '
#             'GROUP BY "abc"."foo" '
#             'HAVING "abc"."buz"=\'fiz\' AND SUM("efg"."bar")>100',
#             str(q),
#         )
#
#     def test_mysql_query_uses_backtick_quote_chars(self):
#         stmt = (
#             MySQLSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual("SELECT `foo` FROM `abc` GROUP BY `foo` HAVING `buz`='fiz'", stmt.sql(self.mysql))
#
#     def test_vertica_query_uses_double_quote_chars(self):
#         stmt = (
#             VerticaSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo" HAVING "buz"=\'fiz\'', stmt.sql(self.mysql))
#
#     def test_mssql_query_uses_double_quote_chars(self):
#         stmt = (
#             MSSQLSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo" HAVING "buz"=\'fiz\'', stmt.sql(self.mysql))
#
#     def test_oracle_query_uses_no_quote_chars(self):
#         stmt = (
#             OracleSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual('SELECT foo FROM abc GROUP BY foo HAVING buz=\'fiz\'', stmt.sql(self.mysql))
#
#     def test_postgres_query_uses_double_quote_chars(self):
#         stmt = (
#             PostgreSQLSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo" HAVING "buz"=\'fiz\'', stmt.sql(self.mysql))
#
#     def test_redshift_query_uses_double_quote_chars(self):
#         stmt = (
#             RedshiftSelectStatement().from_(self.table_abc)
#             .select(self.table_abc.foo)
#             .groupby(self.table_abc.foo)
#             .having(self.table_abc.buz == "fiz")
#         )
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo" HAVING "buz"=\'fiz\'', stmt.sql(self.mysql))
#
#
# class OrderByTests(unittest.TestCase):
#     t = Table("abc")
#
#     def test_orderby_single_field(self):
#         stmt = SelectStatement().from_(self.t).orderby(self.t.foo).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', stmt.sql(self.mysql))
#
#     def test_orderby_multi_fields(self):
#         stmt = SelectStatement().from_(self.t).orderby(self.t.foo, self.t.bar).select(self.t.foo, self.t.bar)
#
#         self.assertEqual('SELECT "foo","bar" FROM "abc" ORDER BY "foo","bar"', stmt.sql(self.mysql))
#
#     def test_orderby_single_str(self):
#         stmt = SelectStatement().from_(self.table_abc).orderby("foo").select(ColumnName("foo"))
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', stmt.sql(self.mysql))
#
#     def test_orderby_asc(self):
#         stmt = SelectStatement().from_(self.t).orderby(self.t.foo, order=Order.asc).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" ASC', stmt.sql(self.mysql))
#
#     def test_orderby_desc(self):
#         stmt = SelectStatement().from_(self.t).orderby(self.t.foo, order=Order.desc).select(self.t.foo)
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo" DESC', stmt.sql(self.mysql))
#
#     def test_orderby_no_alias(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = SelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).orderby(bar)
#
#         self.assertEqual(
#             'SELECT SUM("foo"),"bar" "bar01" FROM "abc" ORDER BY "bar"',
#             q.get_sql(orderby_alias=False),
#         )
#
#     def test_orderby_alias(self):
#         bar = self.t.bar.as_("bar01")
#         stmt = SelectStatement().from_(self.t).select(fn.Sum(self.t.foo), bar).orderby(bar)
#
#         self.assertEqual('SELECT SUM("foo"),"bar" "bar01" FROM "abc" ORDER BY "bar01"', q.get_sql())
#
#
# class AliasTests(unittest.TestCase):
#     t = Table("abc")
#
#     def test_table_field(self):
#         stmt = SelectStatement().from_(self.t).select(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" "bar" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_table_field__multi(self):
#         stmt = SelectStatement().from_(self.t).select(self.t.foo.as_("bar"), self.t.fiz.as_("buz"))
#
#         self.assertEqual('SELECT "foo" "bar","fiz" "buz" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_arithmetic_function(self):
#         stmt = SelectStatement().from_(self.t).select((self.t.foo + self.t.bar).as_("biz"))
#
#         self.assertEqual('SELECT "foo"+"bar" "biz" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_functions_using_as(self):
#         stmt = SelectStatement().from_(self.t).select(fn.Count("*").as_("foo"))
#
#         self.assertEqual('SELECT COUNT(*) "foo" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_functions_using_constructor_param(self):
#         stmt = SelectStatement().from_(self.t).select(fn.Count("*", alias="foo"))
#
#         self.assertEqual('SELECT COUNT(*) "foo" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_function_using_as_nested(self):
#         """
#         We don't show aliases of fields that are arguments of a function.
#         """
#         stmt = SelectStatement().from_(self.t).select(fn.Sqrt(fn.Count("*").as_("foo")).as_("bar"))
#
#         self.assertEqual('SELECT SQRT(COUNT(*)) "bar" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_functions_using_constructor_param_nested(self):
#         """
#         We don't show aliases of fields that are arguments of a function.
#         """
#         stmt = SelectStatement().from_(self.t).select(fn.Sqrt(fn.Count("*", alias="foo"), alias="bar"))
#
#         self.assertEqual('SELECT SQRT(COUNT(*)) "bar" FROM "abc"', stmt.sql(self.mysql))
#
#     def test_ignored_in_where(self):
#         stmt = SelectStatement().from_(self.t).select(self.t.foo).where(self.t.foo.as_("bar") == 1)
#
#         self.assertEqual('SELECT "foo" FROM "abc" WHERE "foo"=1', stmt.sql(self.mysql))
#
#     def test_ignored_in_groupby(self):
#         stmt = SelectStatement().from_(self.t).select(self.t.foo).groupby(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" FROM "abc" GROUP BY "foo"', stmt.sql(self.mysql))
#
#     def test_ignored_in_orderby(self):
#         stmt = SelectStatement().from_(self.t).select(self.t.foo).orderby(self.t.foo.as_("bar"))
#
#         self.assertEqual('SELECT "foo" FROM "abc" ORDER BY "foo"', stmt.sql(self.mysql))
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
#         stmt = SelectStatement().from_(self.t).select(Case().when(self.t.foo == 1, "a").else_(self.t.bar.as_('"buz"')))
#
#         self.assertEqual('SELECT CASE WHEN "foo"=1 THEN \'a\' ELSE "bar" END FROM "abc"', stmt.sql(self.mysql))
#
#     def test_case_using_as(self):
#         stmt = SelectStatement().from_(self.t).select(Case().when(self.t.foo == 1, "a").else_("b").as_("bar"))
#
#         self.assertEqual(
#             'SELECT CASE WHEN "foo"=1 THEN \'a\' ELSE \'b\' END "bar" FROM "abc"',
#             str(q),
#         )
#
#     def test_case_using_constructor_param(self):
#         stmt = SelectStatement().from_(self.t).select(Case(alias="bar").when(self.t.foo == 1, "a").else_("b"))
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
#         self.assertEqual('SELECT "q0"."foo","q1"."bar" FROM "abc" "q0","efg" "q1"', stmt.sql(self.mysql))
#
#     def test_use_aliases_in_groupby_and_orderby(self):
#         table_abc = Table("abc", alias="q0")
#
#         my_foo = table_abc.foo.as_("my_foo")
#         stmt = SelectStatement().from_(table_abc).select(my_foo, table_abc.bar).groupby(my_foo).orderby(my_foo)
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
#         self.assertEqual('SELECT NULL "abcdef"', stmt.sql(self.mysql))
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
#         self.assertEqual('SELECT * FROM "abc" WHERE "foo" IN (SELECT * FROM "efg")', stmt.sql(self.mysql))
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
#             .groupby(self.table_abc.foo)
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

if __name__ == '__main__':
    unittest.main()
