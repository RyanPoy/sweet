from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.terms.name_fn import Name


def test_insert_one_column(visitors):
    stmt = InsertStatement(Name("users")).insert([1])
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` VALUES (1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" VALUES (1)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" VALUES (1)'


def test_insert_one_column_single_element_array(visitors):
    stmt = InsertStatement(Name("users")).insert((1,))
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` VALUES (1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" VALUES (1)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" VALUES (1)'


def test_insert_one_column_multi_element_array(visitors):
    stmt = InsertStatement(Name("users")).insert((1,), (2,))
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` VALUES (1), (2)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" VALUES (1), (2)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" VALUES (1), (2)'


def test_insert_single_row_with_array_value(visitors):
    stmt = InsertStatement(Name("users")).insert([1, ["a", "b", "c"]])
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, ['a', 'b', 'c'])"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, ['a', 'b', 'c'])"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, ['a', 'b', 'c'])"


def test_insert_multiple_rows_with_array_value(visitors):
    stmt = InsertStatement(Name("users")).insert((1, ["a", "b", "c"]), (2, ["c", "d", "e"]))
    assert visitors.mysql.sql(stmt) == """INSERT INTO `users` VALUES (1, ['a', 'b', 'c']), (2, ['c', 'd', 'e'])"""
    assert visitors.sqlite.sql(stmt) == """INSERT INTO "users" VALUES (1, ['a', 'b', 'c']), (2, ['c', 'd', 'e'])"""
    assert visitors.pg.sql(stmt) == """INSERT INTO "users" VALUES (1, ['a', 'b', 'c']), (2, ['c', 'd', 'e'])"""


def test_insert_all_columns(visitors):
    stmt = InsertStatement(Name("users")).insert((1, "a", True))
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1)"


def test_insert_all_columns_single_element(visitors):
    stmt = InsertStatement(Name("users")).insert([1, "a", True])
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1)"


def test_insert_all_columns_multi_rows(visitors):
    stmt = InsertStatement(Name("users")).insert((1, "a", True), (2, "b", False))
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"


def test_insert_all_columns_multi_rows_chained(visitors):
    stmt = InsertStatement(Name("users")).insert((1, "a", True)).insert((2, "b", False))
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"


def test_insert_all_columns_multi_rows_chained_mixed(visitors):
    stmt = InsertStatement(Name("users")).insert((1, "a", True), (2, "b", False)).insert((3, "c", True))
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1)"


def test_insert_all_columns_multi_rows_chained_multiple_rows(visitors):
    stmt = InsertStatement(Name("users")).insert((1, "a", True), (2, "b", False)).insert((3, "c", True), (4, "d", False))

    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1), (4, 'd', 0)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1), (4, 'd', 0)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0), (3, 'c', 1), (4, 'd', 0)"


def test_insert_selected_columns(visitors):
    stmt = InsertStatement(Name("users")).column(Name("foo"), Name("bar"), Name("buz")).insert([1, "a", True])
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` (`foo`, `bar`, `buz`) VALUES (1, \'a\', 1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" ("foo", "bar", "buz") VALUES (1, \'a\', 1)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" ("foo", "bar", "buz") VALUES (1, \'a\', 1)'


def test_insert_empty_columns(visitors):
    stmt = InsertStatement(Name("users")).column().insert([1, "a", True])
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` VALUES (1, \'a\', 1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" VALUES (1, \'a\', 1)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" VALUES (1, \'a\', 1)'


def test_insert_ignore(visitors):
    stmt = InsertStatement(Name("users")).insert([1]).ignore()
    assert visitors.mysql.sql(stmt) == 'INSERT IGNORE INTO `users` VALUES (1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT IGNORE INTO "users" VALUES (1)'
    assert visitors.pg.sql(stmt) == 'INSERT IGNORE INTO "users" VALUES (1)'


def test_insert_column(visitors):
    stmt = InsertStatement(Name("users")).insert([1])
    assert visitors.mysql.sql(stmt) == 'INSERT INTO `users` VALUES (1)'
    assert visitors.sqlite.sql(stmt) == 'INSERT INTO "users" VALUES (1)'
    assert visitors.pg.sql(stmt) == 'INSERT INTO "users" VALUES (1)'


def test_insert_column_with_chain(visitors):
    stmt = InsertStatement(Name("users")).insert([1, "a", True]).insert([2, "b", False])
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0)"


def test_replace_simple(visitors):
    stmt = InsertStatement(Name("users")).replace(["v1", "v2", "v3"])
    assert visitors.mysql.sql(stmt) == "REPLACE INTO `users` VALUES ('v1', 'v2', 'v3')"
    assert visitors.sqlite.sql(stmt) == "REPLACE INTO \"users\" VALUES ('v1', 'v2', 'v3')"
    assert visitors.pg.sql(stmt) == "REPLACE INTO \"users\" VALUES ('v1', 'v2', 'v3')"


def test_insert_column_and_returning(visitors):
    stmt = InsertStatement(Name("users")).insert([1, "a", True]).insert([2, "b", False]).returning(Name("id1"), Name("id2")).returning(Name("id3"))
    assert visitors.mysql.sql(stmt) == "INSERT INTO `users` VALUES (1, 'a', 1), (2, 'b', 0) RETURNING `id1`, `id2`, `id3`"
    assert visitors.sqlite.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0) RETURNING \"id1\", \"id2\", \"id3\""
    assert visitors.pg.sql(stmt) == "INSERT INTO \"users\" VALUES (1, 'a', 1), (2, 'b', 0) RETURNING \"id1\", \"id2\", \"id3\""

# def test_replace_subquery(visitors):
#     table_users, table_def = Tables("abc", "efg")
#     query = Query.into(Name("users")).replace(Query.from_(self.table_def).select("f1", "f2"))
#     expected_output = 'REPLACE INTO "abc" VALUES ((SELECT "f1","f2" FROM "efg"))'
#     assert str(query), expected_output)

#     def test_insert_with_statement(visitors):
#         sub_query = Query().select(Name("users").id).from_(Name("users"))
#         aliased = AliasedQuery('sub_qs')
#
#         q = Query().with_(sub_query, 'sub_qs').into(Name("users")).select(aliased.id).from_(aliased)
#         assert
#             'WITH sub_qs AS (SELECT "id" FROM "abc") INSERT INTO "users" SELECT "sub_qs"."id" FROM sub_qs', str(q)
#         )
#
#
# class PostgresInsertIntoOnConflictTests(unittest.TestCase):
#     table_users = Table("abc")
#
#     def test_insert_on_conflict_do_nothing_field(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).on_conflict(Name("users").id).do_nothing()
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT ("id") DO NOTHING', str(query))
#
#     def test_insert_on_conflict_do_nothing_multiple_fields(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1)
#             .on_conflict(Name("users").id, Name("users").sub_id)
#             .do_nothing()
#         )
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT ("id", "sub_id") DO NOTHING', str(query))
#
#     def test_insert_on_conflict_do_nothing_field_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).on_conflict("id").do_nothing()
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT ("id") DO NOTHING', str(query))
#
#     def test_insert_on_conflict_do_nothing_multiple_fields_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).on_conflict("id", "sub_id").do_nothing()
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT ("id", "sub_id") DO NOTHING', str(query))
#
#     def test_insert_on_conflict_do_nothing_mixed_fields(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).on_conflict("id", Name("users").sub_id).do_nothing()
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT ("id", "sub_id") DO NOTHING', str(query))
#
#     def test_insert_on_conflict_do_update_field(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict(Name("users").id)
#             .do_update(Name("users").name, "m")
#         )
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'m') ON CONFLICT (\"id\") DO UPDATE SET \"name\"='m'",
#             str(query),
#         )
#
#     def test_insert_on_conflict_do_update_multiple_fields(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict(Name("users").id, Name("users").sub_id)
#             .do_update(Name("users").name, "m")
#         )
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'m') ON CONFLICT (\"id\", \"sub_id\") DO UPDATE SET \"name\"='m'",
#             str(query),
#         )
#
#     def test_insert_on_conflict_do_update_field_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1, "m").on_conflict("id").do_update("name", "m")
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'m') ON CONFLICT (\"id\") DO UPDATE SET \"name\"='m'",
#             str(query),
#         )
#
#     def test_insert_on_conflict_do_update_multiple_fields_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1, "m").on_conflict("id", "sub_id").do_update("name", "m")
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'m') ON CONFLICT (\"id\", \"sub_id\") DO UPDATE SET \"name\"='m'",
#             str(query),
#         )
#
#     def test_insert_on_conflict_do_update_multiple_mixed_fields(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict("id", Name("users").sub_id)
#             .do_update("name", "m")
#         )
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'m') ON CONFLICT (\"id\", \"sub_id\") DO UPDATE SET \"name\"='m'",
#             str(query),
#         )
#
#     def test_insert_on_conflict_no_handler(visitors):
#         with self.assertRaises(QueryException):
#             query = str(PostgreSQLQuery.into(Name("users")).insert(1).on_conflict(Name("users").id))
#
#     def test_insert_on_conflict_two_handlers_do_nothing(visitors):
#         with self.assertRaises(QueryException):
#             query = (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1)
#                 .on_conflict("id")
#                 .do_nothing()
#                 .do_update(Name("users").name, "m")
#             )
#
#     def test_insert_on_conflict_two_handlers_do_update(visitors):
#         with self.assertRaises(QueryException):
#             query = (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1)
#                 .on_conflict(Name("users").id)
#                 .do_update(Name("users").name, "m")
#                 .do_nothing()
#             )
#
#     def test_non_insert_on_conflict_do_nothing(visitors):
#         with self.assertRaises(QueryException):
#             query = PostgreSQLQuery.update(Name("users")).set("foo", "bar").on_conflict("id").do_nothing()
#
#     def test_non_insert_on_conflict_do_update(visitors):
#         with self.assertRaises(QueryException):
#             query = (
#                 PostgreSQLQuery.update(Name("users")).set("foo", "bar").on_conflict("id").do_update(["name"], ["m"])
#             )
#
#     def test_insert_on_fieldless_conflict_do_nothing(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).on_conflict(None).do_nothing()
#
#         assert 'INSERT INTO "users" VALUES (1) ON CONFLICT DO NOTHING', str(query))
#
#     def test_insert_on_fieldless_conflict_do_update_field(visitors):
#         with self.assertRaises(QueryException):
#             query = str(
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1, "m")
#                 .on_conflict(None)
#                 .do_update(Name("users").name, "m")
#             )
#
#     def test_on_conflict_from_subquery(visitors):
#         table_bcd = Table('bcd')
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(Name("users").fname, Name("users").lname)
#             .select(table_bcd.fname, table_bcd.lname)
#             .from_(table_bcd)
#             .on_conflict(Name("users").id, Name("users").sub_id)
#             .do_update(Name("users").fname, 1)
#             .do_update(Name("users").lname, table_bcd.lname)
#             .do_update(
#                 Name("users").cname,
#                 Case().when(Name("users").cname.eq('cname'), 'new_name').else_(Name("users").cname),
#             )
#         )
#
#         assert
#             'INSERT INTO "users" VALUES ("fname","lname") '
#             'ON CONFLICT ("id", "sub_id") '
#             'DO UPDATE SET "fname"=1,"lname"="bcd"."lname",'
#             '"cname"=CASE WHEN "abc"."cname"=\'cname\' THEN \'new_name\' ELSE "abc"."cname" END',
#             str(query),
#         )
#
#     def test_on_conflict_where_empty_conflict_fields_do_nothing(visitors):
#         with self.assertRaises(QueryException):
#             (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1, "m")
#                 .on_conflict()
#                 .where(Name("users").abc.eq(0))
#                 .where(Name("users").cde.eq(0))
#                 .do_nothing()
#             )
#
#     def test_on_conflict_where_conflict_fields_do_nothing(visitors):
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict('id')
#             .where(Name("users").abc.eq(0))
#             .where(Name("users").cde.eq(0))
#             .do_nothing()
#         )
#         assert
#             '''INSERT INTO "users" VALUES (1,'m') ON CONFLICT ("id") WHERE "abc"=0 AND "cde"=0 DO NOTHING''', str(qs)
#         )
#
#     def test_on_conflict_where_empty_conflict_fields_do_update(visitors):
#         with self.assertRaises(QueryException):
#             (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1, "m")
#                 .on_conflict()
#                 .where(Name("users").abc.eq(0))
#                 .where(Name("users").cde.eq(0))
#                 .do_update('field', 'val')
#             )
#
#     def test_on_conflict_where_conflict_fields_do_update(visitors):
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict('id')
#             .where(Name("users").abc.eq(0))
#             .where(Name("users").cde.eq(0))
#             .do_update('field', 'val')
#         )
#         assert
#             '''INSERT INTO "users" VALUES (1,'m') ON CONFLICT ("id") WHERE "abc"=0 AND "cde"=0 '''
#             '''DO UPDATE SET "field"='val\'''',
#             str(qs),
#         )
#
#     def test_where_and_on_conflict_where(visitors):
#         table_bcd = Table('bcd')
#
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .select(table_bcd.abc)
#             .from_(table_bcd)
#             .where(table_bcd.abc.eq('1'))
#             .on_conflict('id')
#             .where(Name("users").abc.eq(0))
#             .where(Name("users").cde.eq(0))
#             .do_update('field', 'val')
#         )
#
#         assert
#             'INSERT INTO "users" SELECT "abc" FROM "bcd" WHERE "abc"=\'1\' '
#             'ON CONFLICT ("id") WHERE "abc"=0 AND "cde"=0 DO UPDATE SET "field"=\'val\'',
#             str(qs),
#         )
#
#     def test_on_conflict_do_nothing_where(visitors):
#         with self.assertRaises(QueryException):
#             (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1, "m")
#                 .on_conflict()
#                 .do_nothing()
#                 .where(Name("users").abc.eq(1))
#             )
#
#     def test_empty_on_conflict_do_update_where(visitors):
#         with self.assertRaises(QueryException):
#             (
#                 PostgreSQLQuery.into(Name("users"))
#                 .insert(1, "m")
#                 .on_conflict()
#                 .do_update('abc', 1)
#                 .where(Name("users").abc.eq(1))
#             )
#
#     def test_on_conflict_do_update_where(visitors):
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict("id")
#             .do_update('abc', 1)
#             .where(Name("users").abc.eq(1))
#         )
#         assert
#             'INSERT INTO "users" VALUES (1,\'m\') ON CONFLICT ("id") DO UPDATE SET "abc"=1 WHERE "abc"."abc"=1', str(qs)
#         )
#
#     def test_on_conflict_do_update_with_excluded_where(visitors):
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "m")
#             .on_conflict("id")
#             .do_update('abc')
#             .where(Name("users").abc.eq(1))
#         )
#         assert
#             'INSERT INTO "users" VALUES (1,\'m\') ON CONFLICT ("id") DO UPDATE SET "abc"=EXCLUDED."abc" WHERE '
#             '"abc"."abc"=1',
#             str(qs),
#         )
#
#     def test_on_conflict_where_complex(visitors):
#         table_bcd = Table('bcd')
#
#         qs = (
#             PostgreSQLQuery.into(Name("users"))
#             .select(table_bcd.abc)
#             .from_(table_bcd)
#             .where(table_bcd.abc.eq('1'))
#             .on_conflict('id')
#             .where(Name("users").abc.eq(0))
#             .where(Name("users").cde.eq(0))
#             .do_update('field', 'val')
#             .where(Name("users").id.eq(2))
#             .where(Name("users").sub_id.eq(3))
#         )
#         assert
#             'INSERT INTO "users" SELECT "abc" FROM "bcd" WHERE "abc"=\'1\' '
#             'ON CONFLICT ("id") WHERE "abc"=0 AND "cde"=0 '
#             'DO UPDATE SET "field"=\'val\' WHERE "abc"."id"=2 AND "abc"."sub_id"=3',
#             str(qs),
#         )
#
#
# class PostgresInsertIntoReturningTests(unittest.TestCase):
#     table_users = Table("abc")
#
#     def test_insert_returning_one_field(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning(Name("users").id)
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING "id"', str(query))
#
#     def test_insert_returning_one_field_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning("id")
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING "id"', str(query))
#
#     def test_insert_returning_all_fields(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning(Name("users").star)
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING *', str(query))
#
#     def test_insert_returning_all_fields_and_arithmetics(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1)
#             .returning(Name("users").star, Name("users").f1 + Name("users").f2)
#         )
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING *,"f1"+"f2"', str(query))
#
#     def test_insert_all_columns_multi_rows_chained_returning_star(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "a", True)
#             .insert(2, "b", False)
#             .returning(Name("users").star)
#         )
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'a',true),(2,'b',false) RETURNING *",
#             str(query),
#         )
#
#     def test_insert_all_columns_multi_rows_chained_returning_star_and_id(visitors):
#         query = (
#             PostgreSQLQuery.into(Name("users"))
#             .insert(1, "a", True)
#             .insert(2, "b", False)
#             .returning(
#                 Name("users").name,
#                 Name("users").star,
#                 Name("users").id,
#             )
#         )
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'a',true),(2,'b',false) RETURNING *",
#             str(query),
#         )
#
#     def test_insert_all_columns_multi_rows_chained_returning_star_str(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1, "a", True).insert(2, "b", False).returning("*")
#
#         assert
#             "INSERT INTO \"users\" VALUES (1,'a',true),(2,'b',false) RETURNING *",
#             str(query),
#         )
#
#     def test_insert_all_columns_single_element_arrays(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert((1, "a", True)).returning(Name("users").star)
#
#         assert "INSERT INTO \"users\" VALUES (1,'a',true) RETURNING *", str(query))
#
#     def test_insert_returning_null(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning(None)
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING NULL', str(query))
#
#     def test_insert_returning_tuple(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning((1, 2, 3))
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING (1,2,3)', str(query))
#
#     def test_insert_returning_arithmetics(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning(Name("users").f1 + Name("users").f2)
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING "f1"+"f2"', str(query))
#
#     def test_insert_returning_functions(visitors):
#         query = PostgreSQLQuery.into(Name("users")).insert(1).returning(Cast(Name("users").f1, "int"))
#
#         assert 'INSERT INTO "users" VALUES (1) RETURNING CAST("f1" AS INT)', str(query))
#
#     def test_insert_returning_aggregate(visitors):
#         with self.assertRaises(QueryException):
#             PostgreSQLQuery.into(Name("users")).insert(1).returning(Avg(Name("users").views))
#
#     def test_insert_returning_from_other_table(visitors):
#         table_cba = Table("cba")
#         with self.assertRaises(QueryException):
#             PostgreSQLQuery.into(Name("users")).insert(1).returning(table_cba.id)
#
#
# class InsertIntoOnDuplicateTests(unittest.TestCase):
#     table_users = Table("abc")
#
#     def test_insert_one_column(visitors):
#         query = (
#             MySQLQuery.into(Name("users")).insert(1).on_duplicate_key_update(Name("users").foo, Name("users").foo)
#         )
#         assert
#             "INSERT INTO `abc` VALUES (1) ON DUPLICATE KEY UPDATE `foo`=`foo`",
#             str(query),
#         )
#
#     def test_insert_one_column_using_values(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert(1)
#             .on_duplicate_key_update(Name("users").foo, Values(Name("users").foo))
#         )
#         assert
#             "INSERT INTO `abc` VALUES (1) ON DUPLICATE KEY UPDATE `foo`=VALUES(`foo`)",
#             str(query),
#         )
#
#     def test_insert_one_column_single_element_array(visitors):
#         query = (
#             MySQLQuery.into(Name("users")).insert((1,)).on_duplicate_key_update(Name("users").foo, Name("users").foo)
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1) ON DUPLICATE KEY UPDATE `foo`=`foo`",
#             str(query),
#         )
#
#     def test_insert_one_column_multi_element_array(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert((1,), (2,))
#             .on_duplicate_key_update(Name("users").foo, Name("users").foo)
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1),(2) ON DUPLICATE KEY UPDATE `foo`=`foo`",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_one_with_same_value(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert(1, "a")
#             .on_duplicate_key_update(Name("users").bar, Values(Name("users").bar))
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a') ON DUPLICATE KEY UPDATE `bar`=VALUES(`bar`)",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_one_with_different_value(visitors):
#         query = MySQLQuery.into(Name("users")).insert(1, "a").on_duplicate_key_update(Name("users").bar, "b")
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a') ON DUPLICATE KEY UPDATE `bar`='b'",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_one_with_expression(visitors):
#         query = (
#             MySQLQuery.into(Name("users")).insert(1, 2).on_duplicate_key_update(Name("users").bar, 4 + F("bar"))
#         )  # todo sql expression? not python
#
#         assert
#             "INSERT INTO `abc` VALUES (1,2) ON DUPLICATE KEY UPDATE `bar`=4+`bar`",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_one_with_expression_using_original_field_value(
#         self,
#     ):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert(1, "a")
#             .on_duplicate_key_update(Name("users").bar, fn.Concat(Name("users").bar, "update"))
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a') ON DUPLICATE KEY UPDATE `bar`=CONCAT(`bar`,'update')",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_one_with_expression_using_values(
#         self,
#     ):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert(1, "a")
#             .on_duplicate_key_update(Name("users").bar, fn.Concat(Values(Name("users").bar), "update"))
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a') ON DUPLICATE KEY UPDATE `bar`=CONCAT(VALUES(`bar`),'update')",
#             str(query),
#         )
#
#     def test_insert_multiple_columns_on_duplicate_update_multiple(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert(1, "a", "b")
#             .on_duplicate_key_update(Name("users").bar, "b")
#             .on_duplicate_key_update(Name("users").baz, "c")
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a','b') ON DUPLICATE KEY UPDATE `bar`='b',`baz`='c'",
#             str(query),
#         )
#
#     def test_insert_multi_rows_chained_mixed_on_duplicate_update_multiple(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .insert((1, "a", True), (2, "b", False))
#             .insert(3, "c", True)
#             .on_duplicate_key_update(Name("users").foo, Name("users").foo)
#             .on_duplicate_key_update(Name("users").bar, Values(Name("users").bar))
#         )
#
#         assert
#             "INSERT INTO `abc` VALUES (1,'a',true),(2,'b',false),(3,'c',true) "
#             "ON DUPLICATE KEY UPDATE `foo`=`foo`,`bar`=VALUES(`bar`)",
#             str(query),
#         )
#
#     def test_insert_selected_columns_on_duplicate_update_one(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .column(Name("users").foo, Name("users").bar, Name("users").baz)
#             .insert(1, "a", True)
#             .on_duplicate_key_update(Name("users").baz, False)
#         )
#
#         assert
#             "INSERT INTO `abc` (`foo`,`bar`,`baz`) VALUES (1,'a',true) ON DUPLICATE KEY UPDATE `baz`=false",
#             str(query),
#         )
#
#     def test_insert_selected_columns_on_duplicate_update_multiple(visitors):
#         query = (
#             MySQLQuery.into(Name("users"))
#             .column(Name("users").foo, Name("users").bar, Name("users").baz)
#             .insert(1, "a", True)
#             .on_duplicate_key_update(Name("users").baz, False)
#             .on_duplicate_key_update(Name("users").bar, Values(Name("users").bar))
#         )
#
#         assert
#             "INSERT INTO `abc` (`foo`,`bar`,`baz`) VALUES (1,'a',true) "
#             "ON DUPLICATE KEY UPDATE `baz`=false,`bar`=VALUES(`bar`)",
#             str(query),
#         )
#
#     def test_insert_none_skipped(visitors):
#         query = MySQLQuery.into(Name("users")).insert().on_duplicate_key_update(Name("users").baz, False)
#
#         assert "", str(query))
#
#     def test_insert_ignore(visitors):
#         query = MySQLQuery.into(Name("users")).insert(1).ignore().on_duplicate_key_update(Name("users").baz, False)
#
#         assert
#             "INSERT IGNORE INTO `abc` VALUES (1) ON DUPLICATE KEY UPDATE `baz`=false",
#             str(query),
#         )
#
#
# class InsertSelectFromTests(unittest.TestCase):
#     table_users, table_efg, table_hij = Tables("abc", "efg", "hij")
#
#     def test_insert_star(visitors):
#         query = Query.into(Name("users")).from_(self.table_efg).select("*")
#
#         assert 'INSERT INTO "users" SELECT * FROM "efg"', str(query))
#
#     def test_insert_ignore_star(visitors):
#         query = Query.into(Name("users")).from_(self.table_efg).select("*").ignore()
#
#         assert 'INSERT IGNORE INTO "abc" SELECT * FROM "efg"', str(query))
#
#     def test_insert_from_columns(visitors):
#         query = (
#             Query.into(Name("users"))
#             .from_(self.table_efg)
#             .select(self.table_efg.fiz, self.table_efg.buz, self.table_efg.baz)
#         )
#
#         assert 'INSERT INTO "users" ' 'SELECT "fiz","buz","baz" FROM "efg"', str(query))
#
#     def test_insert_columns_from_star(visitors):
#         query = (
#             Query.into(Name("users"))
#             .column(
#                 Name("users").foo,
#                 Name("users").bar,
#                 Name("users").buz,
#             )
#             .from_(self.table_efg)
#             .select("*")
#         )
#
#         assert 'INSERT INTO "users" ("foo","bar","buz") ' 'SELECT * FROM "efg"', str(query))
#
#     def test_insert_columns_from_columns(visitors):
#         query = (
#             Query.into(Name("users"))
#             .column(Name("users").foo, Name("users").bar, Name("users").buz)
#             .from_(self.table_efg)
#             .select(self.table_efg.fiz, self.table_efg.buz, self.table_efg.baz)
#         )
#
#         assert
#             'INSERT INTO "users" ("foo","bar","buz") ' 'SELECT "fiz","buz","baz" FROM "efg"',
#             str(query),
#         )
#
#     def test_insert_columns_from_columns_with_join(visitors):
#         query = (
#             Query.into(Name("users"))
#             .column(
#                 Name("users").c1,
#                 Name("users").c2,
#                 Name("users").c3,
#                 Name("users").c4,
#             )
#             .from_(self.table_efg)
#             .select(self.table_efg.foo, self.table_efg.bar)
#             .join(self.table_hij)
#             .on(self.table_efg.id == self.table_hij.abc_id)
#             .select(self.table_hij.fiz, self.table_hij.buz)
#         )
#
#         assert
#             'INSERT INTO "users" ("c1","c2","c3","c4") '
#             'SELECT "efg"."foo","efg"."bar","hij"."fiz","hij"."buz" FROM "efg" '
#             'JOIN "hij" ON "efg"."id"="hij"."abc_id"',
#             str(query),
#         )
#
#
# class InsertSubqueryTests(unittest.TestCase):
#     def test_insert_subquery_wrapped_in_brackets(visitors):
#         purchase_order_item, part = Tables("purchase_order_item", "part")
#
#         q = (
#             Query.into(purchase_order_item)
#             .column(purchase_order_item.id_part, purchase_order_item.id_customer)
#             .insert(
#                 Query.from_(part).select(part.part_id).where(part.part_number == "FOOBAR"),
#                 12345,
#             )
#         )
#
#         assert
#             'INSERT INTO "purchase_order_item" '
#             '("id_part","id_customer") '
#             "VALUES "
#             '((SELECT "part_id" FROM "part" WHERE "part_number"=\'FOOBAR\'),12345)',
#             str(q),
#         )
#
#
# class SelectIntoTests(unittest.TestCase):
#     table_users, table_efg, table_hij = Tables("abc", "efg", "hij")
#
#     def test_select_star_into(visitors):
#         query = Query.from_(Name("users")).select("*").into(self.table_efg)
#
#         assert 'SELECT * INTO "efg" FROM "abc"', str(query))
#
#     def test_select_columns_into(visitors):
#         query = (
#             Query.from_(Name("users"))
#             .select(Name("users").foo, Name("users").bar, Name("users").buz)
#             .into(self.table_efg)
#         )
#
#         assert 'SELECT "foo","bar","buz" INTO "efg" FROM "abc"', str(query))
#
#     def test_select_columns_into_with_join(visitors):
#         query = (
#             Query.from_(Name("users"))
#             .select(Name("users").foo, Name("users").bar)
#             .join(self.table_hij)
#             .on(Name("users").id == self.table_hij.abc_id)
#             .select(self.table_hij.fiz, self.table_hij.buz)
#             .into(self.table_efg)
#         )
#
#         assert
#             'SELECT "abc"."foo","abc"."bar","hij"."fiz","hij"."buz" '
#             'INTO "efg" FROM "abc" '
#             'JOIN "hij" ON "abc"."id"="hij"."abc_id"',
#             str(query),
#         )
