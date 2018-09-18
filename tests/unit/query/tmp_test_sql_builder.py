#coding: utf8
import unitest


class TmpSQLBuilderTest(unitest.TestCase):

    # def test_unions(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? UNION SELECT * FROM "users" WHERE "id" = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
    #     self.assertEqual(
    #         '(SELECT * FROM `users` WHERE `id` = %s) UNION (SELECT * FROM `users` WHERE `id` = %s)',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_union_alls(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? UNION ALL SELECT * FROM "users" WHERE "id" = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_multiple_unions(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.union(self.get_builder().select('*').from_('users').where('id', '=', 3))
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION SELECT * FROM "users" WHERE "id" = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2, 3], builder.get_bindings())

    # def test_multiple_union_alls(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 3))
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION ALL SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION ALL SELECT * FROM "users" WHERE "id" = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2, 3], builder.get_bindings())

    # def test_union_order_bys(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.order_by('id', 'desc')
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION SELECT * FROM "users" WHERE "id" = ? '
    #         'ORDER BY "id" DESC',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_union_limits_and_offsets(self):
    #     builder = self.get_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.skip(5).take(10)
    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE "id" = ? '
    #         'UNION SELECT * FROM "users" WHERE "id" = ? '
    #         'LIMIT 10 OFFSET 5',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_mysql_union_order_bys(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.order_by('id', 'desc')
    #     self.assertEqual(
    #         '(SELECT * FROM `users` WHERE `id` = %s) '
    #         'UNION (SELECT * FROM `users` WHERE `id` = %s) '
    #         'ORDER BY `id` DESC',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_mysql_union_limits_and_offsets(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where('id', '=', 1)
    #     builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
    #     builder.skip(5).take(10)
    #     self.assertEqual(
    #         '(SELECT * FROM `users` WHERE `id` = %s) '
    #         'UNION (SELECT * FROM `users` WHERE `id` = %s) '
    #         'LIMIT 10 OFFSET 5',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1, 2], builder.get_bindings())

    # def test_basic_select_distinct(self):
    #     builder = self.get_builder()
    #     builder.distinct().select('foo', 'bar').from_('users')

    #     self.assertEqual(
    #         'SELECT DISTINCT "foo", "bar" FROM "users"',
    #         builder.to_sql()
    #     )

    # def test_mysql_wrapping_protects_qutotation_marks(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('some`table')

    #     self.assertEqual(
    #         'SELECT * FROM `some``table`',
    #         builder.to_sql()
    #     )

    # def test_where_day_mysql(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where_day('created_at', '=', 1)

    #     self.assertEqual(
    #         'SELECT * FROM `users` WHERE DAY(`created_at`) = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1], builder.get_bindings())

    # def test_where_month_mysql(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where_month('created_at', '=', 5)

    #     self.assertEqual(
    #         'SELECT * FROM `users` WHERE MONTH(`created_at`) = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([5], builder.get_bindings())

    # def test_where_year_mysql(self):
    #     builder = self.get_mysql_builder()
    #     builder.select('*').from_('users').where_year('created_at', '=', 2014)

    #     self.assertEqual(
    #         'SELECT * FROM `users` WHERE YEAR(`created_at`) = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([2014], builder.get_bindings())

    # def test_where_day_postgres(self):
    #     builder = self.get_postgres_builder()
    #     builder.select('*').from_('users').where_day('created_at', '=', 1)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE DAY("created_at") = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1], builder.get_bindings())

    # def test_where_month_postgres(self):
    #     builder = self.get_postgres_builder()
    #     builder.select('*').from_('users').where_month('created_at', '=', 5)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE MONTH("created_at") = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([5], builder.get_bindings())

    # def test_where_year_postgres(self):
    #     builder = self.get_postgres_builder()
    #     builder.select('*').from_('users').where_year('created_at', '=', 2014)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE YEAR("created_at") = %s',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([2014], builder.get_bindings())

    # def test_where_day_sqlite(self):
    #     builder = self.get_sqlite_builder()
    #     builder.select('*').from_('users').where_day('created_at', '=', 1)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE strftime(\'%d\', "created_at") = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([1], builder.get_bindings())

    # def test_where_month_sqlite(self):
    #     builder = self.get_sqlite_builder()
    #     builder.select('*').from_('users').where_month('created_at', '=', 5)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE strftime(\'%m\', "created_at") = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([5], builder.get_bindings())

    # def test_where_year_sqlite(self):
    #     builder = self.get_sqlite_builder()
    #     builder.select('*').from_('users').where_year('created_at', '=', 2014)

    #     self.assertEqual(
    #         'SELECT * FROM "users" WHERE strftime(\'%Y\', "created_at") = ?',
    #         builder.to_sql()
    #     )
    #     self.assertEqual([2014], builder.get_bindings())
    
