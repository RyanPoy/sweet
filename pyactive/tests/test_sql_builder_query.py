#coding: utf8
from pyactive.query.sql_builder import SQLBuilder
from pyactive.query.join_clause import JoinClause
import unittest
import fudge


class SQLBuilderQueryTestCase(unittest.TestCase):
    
    def get_builder(self, conn=None):
        return SQLBuilder(conn)

    def test_basic_select(self):
        builder = self.get_builder()
        builder.select('*').from_('users')
        
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)

    def test_alias_select(self):
        builder = self.get_builder()
        builder.select('foo as bar').from_('users')
        sql, params = builder.to_sql()
        self.assertEqual('SELECT `foo` AS `bar` FROM `users`', sql )
        self.assertEqual([], params)

        builder = self.get_builder()
        builder.select('x.y AS foo.bar').from_('baz')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT `x`.`y` AS `foo`.`bar` FROM `baz`', sql )
        self.assertEqual([], params)

    def test_adding_selects(self):
        builder = self.get_builder()
        builder.select('foo').select('bar').select('baz', 'boom').select('x.y AS foo.bar').from_('users')
        
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT `users`.`foo`, `users`.`bar`, `users`.`baz`, `users`.`boom`, `x`.`y` AS `foo`.`bar` FROM `users`', sql )
        self.assertEqual([], params)

    def test_basic_select_distinct(self):
        builder = self.get_builder()
        builder.distinct().select('foo', 'bar').from_('users')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT DISTINCT `users`.`foo`, `users`.`bar` FROM `users`', sql )
        self.assertEqual([], params) 

    def test_basic_where(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=1)
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ?', sql )
        self.assertEqual([1], params)
        
    def test_where_in(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=[1, 2])
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` IN (?, ?)', sql )
        self.assertEqual([1, 2], params)
        
    def test_raw_where(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE id = ? or email = ?', sql )
        self.assertEqual([1, 'foo'], params)
         
    def test_multi_contidions_where(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=1).where('email in (?, ?)', 'foo', 'bar')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ? AND email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)

    def test_empty_where_in(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=[])
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)
 
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=[]).where('email in (?, ?)', 'foo', 'bar')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE email in (?, ?)', sql )
        self.assertEqual(['foo', 'bar'], params)
        
    def test_basic_where_null(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=None)
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `users`.`id` IS NULL', sql)
        self.assertEqual([], params)

    def test_group_by(self):
        builder = self.get_builder()
        builder.select('*').from_('users').group_by('id', 'email').group_by('name')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` GROUP BY `users`.`id`, `users`.`email`, `users`.`name`', sql )
        self.assertEqual( [], params )

        builder = self.get_builder()
        builder.select('*').from_('users').group_by('id, email').group_by('name')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` GROUP BY `users`.`id`, `users`.`email`, `users`.`name`', sql )
        self.assertEqual( [], params )

    def test_order_by(self):
        builder = self.get_builder()
        builder.select('*').from_('users').order_by('email').order_by('age desc, id ASC')
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` ORDER BY `users`.`email`, `users`.`age` DESC, `users`.`id` ASC', sql)
        self.assertEqual( [], params )

        builder = self.get_builder()
        builder.select('*').from_('users').order_by('email').order_by('age DESC', 'id ASC')
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` ORDER BY `users`.`email`, `users`.`age` DESC, `users`.`id` ASC', sql)
        self.assertEqual( [], params )
 
    def test_basic_having(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=1)
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` = ?', sql )
        self.assertEqual([1], params)
        
    def test_having_in(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=[1, 2])
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` IN (?, ?)', sql )
        self.assertEqual([1, 2], params)
        
    def test_raw_having(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having('id = ? or email = ?', 1, 'foo')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING id = ? or email = ?', sql )
        self.assertEqual([1, 'foo'], params)

    def test_multi_contidions_having(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=1).having('email in (?, ?)', 'foo', 'bar')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` = ? AND email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)

    def test_empty_having_in(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=[])
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)
 
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=[]).having('email in (?, ?)', 'foo', 'bar')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING email in (?, ?)', sql )
        self.assertEqual(['foo', 'bar'], params)
        
    def test_basic_having_null(self):
        builder = self.get_builder()
        builder.select('*').from_('users').having(id=None)
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `users`.`id` IS NULL', sql)
        self.assertEqual([], params)
        
    def test_multi_contidions_having_follow_group(self):
        builder = self.get_builder()
        builder.select('*').from_('users').where(id=1).having('email in (?, ?)', 'foo', 'bar').group_by('name')
        sql, params = builder.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ? GROUP BY `users`.`name` HAVING email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)
 
    def test_limits_and_offsets(self):
        builder = self.get_builder()
        builder.select('*').from_('users').offset(5).limit(10)
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 5', sql)
        self.assertEqual([], params)

    def test_page(self):
        builder = self.get_builder()
        builder.select('*').from_('users').page(5, 10)
        sql, params = builder.to_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 50', sql)
        self.assertEqual([], params)

    def test_basic_joins(self):
        builder = self.get_builder()
        builder.select('*').from_('users')\
            .join('contacts', 'users.id = contacts.id')\
            .left_join('photos', 'users.id=photos.user_id')\
            .right_join('cards', 'users.id=?', 'cards.user_id')
        sql, params = builder.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` '
            'INNER JOIN `contacts` ON users.id = contacts.id '
            'LEFT JOIN `photos` ON users.id=photos.user_id '
            'RIGHT JOIN `cards` ON users.id=?',
            sql
        )
        self.assertEqual(['cards.user_id'], params)

    def test_complex_joins(self):
        builder = self.get_builder()
        builder.select('*').from_('users').join(
            JoinClause('contacts')
            .on('users.id=contacts.id')
            .on('users.name=?', 'contacts.name')
        )
        sql, params = builder.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` '
            'INNER JOIN `contacts` ON users.id=contacts.id '
            'AND users.name=?',
            sql
        )
        self.assertEqual(['contacts.name'], params)

#     def test_raw_expression_in_select(self):
#         builder = self.get_builder()
#         builder.select(QueryExpression('substr(foo, 6)')).from_('users')
#         self.assertEqual('SELECT substr(foo, 6) FROM "users"', builder.to_sql())
# 

    def test_first_return_none_result_if_can_not_found(self):
        results = []
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE id = ? or email = ? LIMIT 1', 1, 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        limit, offset = builder._limit, builder._offset
        
        builder.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        self.assertEqual(None, builder.first() )
        self.assertEqual(limit, builder._limit)
        self.assertEqual(offset, builder._offset)
        
    def test_first_return_first_result(self):
        results = [
            {'id': 1, 'email': 'foo'}
        ]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE id = ? or email = ? LIMIT 1', 1, 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        limit, offset = builder._limit, builder._offset
        
        builder.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        self.assertEqual(results[0], builder.first() )
        self.assertEqual(limit, builder._limit)
        self.assertEqual(offset, builder._offset)
            
    def test_all_return_all_results(self):
        results = [
            {'id': 1, 'tag': 'foo'},
            {'id': 2, 'tag': 'boom'},
            {'id': 3, 'tag': 'boom'},
            {'id': 4, 'tag': 'foo'},
        ]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE `users`.`tag` IN (?, ?)', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(results, builder.all() )
    
#     def test_insert_method(self):
#         builder = self.get_builder()
#         query = 'INSERT INTO "users" ("email") VALUES (?)'
#         builder.get_connection().insert.return_value = True
#         result = builder.from_('users').insert({'email': 'foo'})
#         builder.get_connection().insert.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertTrue(result)
# 
#     def test_insert_method_with_keyword_arguments(self):
#         builder = self.get_builder()
#         query = 'INSERT INTO "users" ("email") VALUES (?)'
#         builder.get_connection().insert.return_value = True
#         result = builder.from_('users').insert({'email': 'foo'})
#         builder.get_connection().insert.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertTrue(result)
# 
#     def test_sqlite_multiple_insert(self):
#         builder = self.get_sqlite_builder()
#         query = 'INSERT INTO "users" ("email", "name") ' \
#                 'SELECT ? AS "email", ? AS "name" UNION ALL SELECT ? AS "email", ? AS "name"'
#         builder.get_connection().insert.return_value = True
#         result = builder.from_('users').insert([
#             {'email': 'foo', 'name': 'john'},
#             {'email': 'bar', 'name': 'jane'}
#         ])
#         builder.get_connection().insert.assert_called_once_with(
#             query, ['foo', 'john', 'bar', 'jane']
#         )
#         self.assertTrue(result)
# 
#     def test_insert_get_id_method(self):
#         builder = self.get_builder()
#         builder.get_processor().process_insert_get_id.return_value = 1
#         result = builder.from_('users').insert_get_id({
#             'email': 'foo',
#             'bar': QueryExpression('bar')
#         })
#         builder.get_processor().process_insert_get_id.assert_called_once_with(
#             builder, 'INSERT INTO "users" ("bar", "email") VALUES (bar, ?)', ['foo'], None
#         )
#         self.assertEqual(1, result)
# 
#     def test_insert_get_id_with_sequence(self):
#         builder = self.get_builder()
#         builder.get_processor().process_insert_get_id.return_value = 1
#         result = builder.from_('users').insert_get_id({
#             'email': 'foo',
#             'bar': QueryExpression('bar')
#         }, 'id')
#         builder.get_processor().process_insert_get_id.assert_called_once_with(
#             builder, 'INSERT INTO "users" ("bar", "email") VALUES (bar, ?)', ['foo'], 'id'
#         )
#         self.assertEqual(1, result)
# 
#     def test_insert_get_id_respects_raw_bindings(self):
#         builder = self.get_builder()
#         builder.get_processor().process_insert_get_id.return_value = 1
#         result = builder.from_('users').insert_get_id({
#             'email': QueryExpression('CURRENT_TIMESTAMP'),
#         })
#         builder.get_processor().process_insert_get_id.assert_called_once_with(
#             builder, 'INSERT INTO "users" ("email") VALUES (CURRENT_TIMESTAMP)', [], None
#         )
#         self.assertEqual(1, result)
# 
#     def test_update(self):
#         builder = self.get_builder()
#         query = 'UPDATE "users" SET "email" = ?, "name" = ? WHERE "id" = ?'
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update(email='foo', name='bar')
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
#         
#         builder = self.get_mysql_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'UPDATE `users` SET `email` = %s, `name` = %s WHERE `id` = %s' % (marker, marker, marker)
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update(email='foo', name='bar')
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_with_dictionaries(self):
#         builder = self.get_builder()
#         query = 'UPDATE "users" SET "email" = ?, "name" = ? WHERE "id" = ?'
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update({'email': 'foo', 'name': 'bar'})
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
#         builder = self.get_builder()
# 
#         query = 'UPDATE "users" SET "email" = ?, "name" = ? WHERE "id" = ?'
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update({'email': 'foo'}, name='bar')
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_with_joins(self):
#         builder = self.get_builder()
#         query = 'UPDATE "users" ' \
#                 'INNER JOIN "orders" ON "users"."id" = "orders"."user_id" ' \
#                 'SET "email" = ?, "name" = ? WHERE "id" = ?'
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users')\
#             .join('orders', 'users.id', '=', 'orders.user_id')\
#             .where('id', '=', 1)\
#             .update(email='foo', name='bar')
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_on_postgres(self):
#         builder = self.get_postgres_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'UPDATE "users" SET "email" = %s, "name" = %s WHERE "id" = %s' % (marker, marker, marker)
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update(email='foo', name='bar')
#         builder.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_with_joins_on_postgres(self):
#         builder = self.get_postgres_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'UPDATE "users" ' \
#                 'SET "email" = %s, "name" = %s ' \
#                 'FROM "orders" WHERE "id" = %s AND "users"."id" = "orders"."user_id"'\
#                 % (marker, marker, marker)
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users')\
#             .join('orders', 'users.id', '=', 'orders.user_id')\
#             .where('id', '=', 1)\
#             .update(email='foo', name='bar')
#         builder.get_connection().update.assert_called_once_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_respects_raw(self):
#         builder = self.get_builder()
#         marker = '?'
#         query = 'UPDATE "users" SET "email" = foo, "name" = %s WHERE "id" = %s' % (marker, marker)
#         builder.get_connection().update.return_value = 1
#         result = builder.from_('users').where('id', '=', 1).update(email=QueryExpression('foo'), name='bar')
#         builder.get_connection().update.assert_called_once_with(
#             query, ['bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_delete(self):
#         builder = self.get_builder()
#         query = 'DELETE FROM "users" WHERE "email" = ?'
#         builder.get_connection().delete.return_value = 1
#         result = builder.from_('users').where('email', '=', 'foo').delete()
#         builder.get_connection().delete.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertEqual(1, result)
# 
#         builder = self.get_builder()
#         query = 'DELETE FROM "users" WHERE "id" = ?'
#         builder.get_connection().delete.return_value = 1
#         result = builder.from_('users').delete(1)
#         builder.get_connection().delete.assert_called_once_with(
#             query, [1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_delete_with_join(self):
#         builder = self.get_mysql_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'DELETE `users` FROM `users` ' \
#                 'INNER JOIN `contacts` ON `users`.`id` = `contacts`.`id` WHERE `email` = %s' % marker
#         builder.get_connection().delete.return_value = 1
#         result = builder.from_('users')\
#             .join('contacts', 'users.id', '=', 'contacts.id')\
#             .where('email', '=', 'foo')\
#             .delete()
#         builder.get_connection().delete.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertEqual(1, result)
# 
#         builder = self.get_mysql_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'DELETE `users` FROM `users` ' \
#                 'INNER JOIN `contacts` ON `users`.`id` = `contacts`.`id` WHERE `id` = %s' % marker
#         builder.get_connection().delete.return_value = 1
#         result = builder.from_('users')\
#             .join('contacts', 'users.id', '=', 'contacts.id')\
#             .delete(1)
#         builder.get_connection().delete.assert_called_once_with(
#             query, [1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_truncate(self):
#         builder = self.get_builder()
#         query = 'TRUNCATE "users"'
#         builder.from_('users').truncate()
#         builder.get_connection().statement.assert_called_once_with(
#             query, []
#         )
# 
#         builder = self.get_sqlite_builder()
#         builder.from_('users')
#         self.assertEqual({
#             'DELETE FROM sqlite_sequence WHERE name = ?': ['users'],
#             'DELETE FROM "users"': []
#         }, builder.get_grammar().compile_truncate(builder))
# 
#     def test_postgres_insert_get_id(self):
#         builder = self.get_postgres_builder()
#         marker = builder.get_grammar().get_marker()
#         query = 'INSERT INTO "users" ("email") VALUES (%s) RETURNING "id"' % marker
#         builder.get_processor().process_insert_get_id.return_value = 1
#         result = builder.from_('users').insert_get_id({'email': 'foo'}, 'id')
#         builder.get_processor().process_insert_get_id.assert_called_once_with(
#             builder, query, ['foo'], 'id'
#         )
#         self.assertEqual(1, result)
# 
#     def test_mysql_wrapping(self):
#         builder = self.get_mysql_builder()
#         builder.select('*').from_('users')
#         self.assertEqual(
#             'SELECT * FROM `users`',
#             builder.to_sql()
#         )
# 
#     def test_merge_wheres_can_merge_wheres_and_bindings(self):
#         builder = self.get_builder()
#         builder.wheres = ['foo']
#         builder.merge_wheres(['wheres'], ['foo', 'bar'])
#         self.assertEqual(['foo', 'wheres'], builder.wheres)
#         self.assertEqual(['foo', 'bar'], builder.get_bindings())
# 
#     def test_where_with_null_second_parameter(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('foo', None)
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "foo" IS NULL',
#             builder.to_sql()
#         )
# 
#     def test_dynamic_where(self):
#         method = 'where_foo_bar_and_baz_or_boom'
#         parameters = ['john', 'jane', 'bam']
# 
#         builder = self.get_builder()
#         builder.where = mock.MagicMock(return_value=builder)
# 
#         getattr(builder, method)(*parameters)
# 
#         builder.where.assert_has_calls([
#             mock.call('foo_bar', '=', parameters[0], 'and'),
#             mock.call('baz', '=', parameters[1], 'and'),
#             mock.call('boom', '=', parameters[2], 'or')
#         ])
# 
#     def test_dynamic_where_is_not_greedy(self):
#         method = 'where_ios_version_and_android_version_or_orientation'
#         parameters = ['6.1', '4.2', 'Vertical']
# 
#         builder = self.get_builder()
#         builder.where = mock.MagicMock(return_value=builder)
# 
#         getattr(builder, method)(*parameters)
# 
#         builder.where.assert_has_calls([
#             mock.call('ios_version', '=', parameters[0], 'and'),
#             mock.call('android_version', '=', parameters[1], 'and'),
#             mock.call('orientation', '=', parameters[2], 'or')
#         ])
# 
#     def test_call_triggers_dynamic_where(self):
#         builder = self.get_builder()
# 
#         self.assertEqual(builder, builder.where_foo_and_bar('baz', 'boom'))
#         self.assertEqual(2, len(builder.wheres))
# 
#     def test_builder_raises_exception_with_undefined_method(self):
#         builder = self.get_builder()
# 
#         try:
#             builder.do_not_exist()
#             self.fail('Builder did not raise and AttributeError exception')
#         except AttributeError:
#             self.assertTrue(True)
# 
#     def test_mysql_lock(self):
#         builder = self.get_mysql_builder()
#         marker = builder.get_grammar().get_marker()
#         builder.select('*').from_('foo').where('bar', '=', 'baz').lock()
#         self.assertEqual(
#             'SELECT * FROM `foo` WHERE `bar` = %s FOR UPDATE' % marker,
#             builder.to_sql()
#         )
#         self.assertEqual(['baz'], builder.get_bindings())
# 
#         builder = self.get_mysql_builder()
#         marker = builder.get_grammar().get_marker()
#         builder.select('*').from_('foo').where('bar', '=', 'baz').lock(False)
#         self.assertEqual(
#             'SELECT * FROM `foo` WHERE `bar` = %s LOCK IN SHARE MODE' % marker,
#             builder.to_sql()
#         )
#         self.assertEqual(['baz'], builder.get_bindings())
# 
#     def test_postgres_lock(self):
#         builder = self.get_postgres_builder()
#         marker = builder.get_grammar().get_marker()
#         builder.select('*').from_('foo').where('bar', '=', 'baz').lock()
#         self.assertEqual(
#             'SELECT * FROM "foo" WHERE "bar" = %s FOR UPDATE' % marker,
#             builder.to_sql()
#         )
#         self.assertEqual(['baz'], builder.get_bindings())
# 
#         builder = self.get_postgres_builder()
#         marker = builder.get_grammar().get_marker()
#         builder.select('*').from_('foo').where('bar', '=', 'baz').lock(False)
#         self.assertEqual(
#             'SELECT * FROM "foo" WHERE "bar" = %s FOR SHARE' % marker,
#             builder.to_sql()
#         )
#         self.assertEqual(['baz'], builder.get_bindings())
# 
#     def test_binding_order(self):
#         expected_sql = 'SELECT * FROM "users" ' \
#                        'INNER JOIN "othertable" ON "bar" = ? ' \
#                        'WHERE "registered" = ? ' \
#                        'GROUP BY "city" ' \
#                        'HAVING "population" > ? ' \
#                        'ORDER BY match ("foo") against(?)'
#         expected_bindings = ['foo', True, 3, 'bar']
# 
#         builder = self.get_builder()
#         builder.select('*').from_('users')\
#             .order_by_raw('match ("foo") against(?)', ['bar'])\
#             .where('registered', True)\
#             .group_by('city')\
#             .having('population', '>', 3)\
#             .join(JoinClause('othertable').where('bar', '=', 'foo'))
# 
#         self.assertEqual(expected_sql, builder.to_sql())
#         self.assertEqual(expected_bindings, builder.get_bindings())
# 
#     def test_add_binding_with_list_merges_bindings(self):
#         builder = self.get_builder()
#         builder.add_binding(['foo', 'bar'])
#         builder.add_binding(['baz'])
#         self.assertEqual(['foo', 'bar', 'baz'], builder.get_bindings())
# 
#     def test_add_binding_with_list_merges_bindings_in_correct_order(self):
#         builder = self.get_builder()
#         builder.add_binding(['bar', 'baz'], 'having')
#         builder.add_binding(['foo'], 'where')
#         self.assertEqual(['foo', 'bar', 'baz'], builder.get_bindings())
# 
#     def test_merge_builders(self):
#         builder = self.get_builder()
#         builder.add_binding('foo', 'where')
#         builder.add_binding('baz', 'having')
#         other_builder = self.get_builder()
#         other_builder.add_binding('bar', 'where')
#         builder.merge_bindings(other_builder)
#         self.assertEqual(['foo', 'bar', 'baz'], builder.get_bindings())
# 
#     def test_sub_select(self):
#         builder = self.get_builder()
#         marker = builder.get_grammar().get_marker()
#         expected_sql = 'SELECT "foo", "bar", (SELECT "baz" FROM "two" WHERE "subkey" = %s) AS "sub" ' \
#                        'FROM "one" WHERE "key" = %s' % (marker, marker)
#         expected_bindings = ['subval', 'val']
# 
#         builder.from_('one').select('foo', 'bar').where('key', '=', 'val')
#         builder.select_sub(builder.new_query().from_('two').select('baz').where('subkey', '=', 'subval'), 'sub')
#         self.assertEqual(expected_sql, builder.to_sql())
#         self.assertEqual(expected_bindings, builder.get_bindings())
# 
#     def test_chunk(self):
#         builder = self.get_builder()
#         results = [
#             {'foo': 'bar'},
#             {'foo': 'baz'},
#             {'foo': 'bam'},
#             {'foo': 'boom'}
#         ]
# 
#         def select(query, bindings, _):
#             index = int(re.search('OFFSET (\d+)', query).group(1))
#             limit = int(re.search('LIMIT (\d+)', query).group(1))
# 
#             if index >= len(results):
#                 return []
# 
#             return results[index:index + limit]
# 
#         builder.get_connection().select.side_effect = select
# 
#         builder.get_processor().process_select = mock.MagicMock(side_effect=lambda builder_, results_: results_)
# 
#         i = 0
#         for users in builder.from_('users').chunk(1):
#             self.assertEqual(users[0], results[i])
# 
#             i += 1
# 
#         builder = self.get_builder()
#         results = [
#             {'foo': 'bar'},
#             {'foo': 'baz'},
#             {'foo': 'bam'},
#             {'foo': 'boom'}
#         ]
# 
#         builder.get_connection().select.side_effect = select
# 
#         builder.get_processor().process_select = mock.MagicMock(side_effect=lambda builder_, results_: results_)
# 
#         for users in builder.from_('users').chunk(2):
#             self.assertEqual(2, len(users))
# 
#     def test_not_specifying_columns_sects_all(self):
#         builder = self.get_builder()
#         builder.from_('users')
# 
#         self.assertEqual(
#             'SELECT * FROM "users"',
#             builder.to_sql()
#         )
# 
#     def get_mysql_builder(self):
#         grammar = MySQLQueryGrammar()
#         processor = MockProcessor().prepare_mock()
#         connection = MockConnection().prepare_mock()
# 
#         return QueryBuilder(connection, grammar, processor)
# 
#     def get_sqlite_builder(self):
#         grammar = SQLiteQueryGrammar()
#         processor = MockProcessor().prepare_mock()
#         connection = MockConnection().prepare_mock()
# 
#         return QueryBuilder(connection, grammar, processor)
# 
#     def get_postgres_builder(self):
#         grammar = PostgresQueryGrammar()
#         processor = MockProcessor().prepare_mock()
#         connection = MockConnection().prepare_mock()
# 
#         return QueryBuilder(connection, grammar, processor)
# 
#     def get_builder(self):
#         grammar = QueryGrammar()
#         processor = MockProcessor().prepare_mock()
#         connection = MockConnection().prepare_mock()
# 
#         return QueryBuilder(connection, grammar, processor)
#
#
#     def test_unions(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? UNION SELECT * FROM "users" WHERE "id" = ?',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#         builder = self.get_mysql_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) UNION (SELECT * FROM `users` WHERE `id` = %s)',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_union_alls(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? UNION ALL SELECT * FROM "users" WHERE "id" = ?',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_multiple_unions(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         builder.union(self.get_builder().select('*').from_('users').where('id', '=', 3))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ?',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2, 3], builder.get_bindings())
# 
#     def test_multiple_union_alls(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         builder.union_all(self.get_builder().select('*').from_('users').where('id', '=', 3))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION ALL SELECT * FROM "users" WHERE "id" = ? '
#             'UNION ALL SELECT * FROM "users" WHERE "id" = ?',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2, 3], builder.get_bindings())
# 
#     def test_union_order_bys(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         builder.order_by('id', 'desc')
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'ORDER BY "id" DESC',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_union_limits_and_offsets(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_builder().select('*').from_('users').where('id', '=', 2))
#         builder.skip(5).take(10)
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'LIMIT 10 OFFSET 5',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_mysql_union_order_bys(self):
#         builder = self.get_mysql_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         builder.order_by('id', 'desc')
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) '
#             'UNION (SELECT * FROM `users` WHERE `id` = %s) '
#             'ORDER BY `id` DESC',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_mysql_union_limits_and_offsets(self):
#         builder = self.get_mysql_builder()
#         builder.select('*').from_('users').where('id', '=', 1)
#         builder.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         builder.skip(5).take(10)
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) '
#             'UNION (SELECT * FROM `users` WHERE `id` = %s) '
#             'LIMIT 10 OFFSET 5',
#             builder.to_sql()
#         )
#         self.assertEqual([1, 2], builder.get_bindings())
# 
#     def test_sub_select_where_in(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where_in(
#             'id',
#             self.get_builder().select('id').from_('users').where('age', '>', 25).take(3)
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" IN (SELECT "id" FROM "users" WHERE "age" > ? LIMIT 3)',
#             builder.to_sql()
#         )
#         self.assertEqual([25], builder.get_bindings())
# 
#         builder = self.get_builder()
#         builder.select('*').from_('users').where_not_in(
#             'id',
#             self.get_builder().select('id').from_('users').where('age', '>', 25).take(3)
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" NOT IN (SELECT "id" FROM "users" WHERE "age" > ? LIMIT 3)',
#             builder.to_sql()
#         )
#         self.assertEqual([25], builder.get_bindings())
#     def test_nested_wheres(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where(email='foo').where(
#             builder.new_instance().where(name='bar').where(age=25)
#         )
#         sql, params = builder.to_sql()
#         self.assertEqual(
#             'SELECT * FROM `users` WHERE `users`.`email` = ? AND (`users`.`name` = ? AND `users`.`age` = ?)',
#             builder.to_sql()
#         )
#         self.assertEqual(['foo', 'bar', 25], builder.get_bindings())
#  
#     def test_full_sub_selects(self):
#         builder = self.get_builder()
#         builder.select('*').from_('users').where('email', '=', 'foo').or_where(
#             'id', '=', builder.new_query().select(QueryExpression('max(id)')).from_('users').where('email', '=', 'bar')
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "email" = ? OR "id" = (SELECT max(id) FROM "users" WHERE "email" = ?)',
#             builder.to_sql()
#         )
#         self.assertEqual(['foo', 'bar'], builder.get_bindings())
# 
#     def test_where_exists(self):
#         builder = self.get_builder()
#         builder.select('*').from_('orders').where_exists(
#             self.get_builder().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" '
#             'WHERE EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             builder.to_sql()
#         )
#         self.assertEqual([], builder.get_bindings())
# 
#         builder = self.get_builder()
#         builder.select('*').from_('orders').where_not_exists(
#             self.get_builder().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" '
#             'WHERE NOT EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             builder.to_sql()
#         )
#         self.assertEqual([], builder.get_bindings())
# 
#         builder = self.get_builder()
#         builder.select('*').from_('orders').where('id', '=', 1).or_where_exists(
#             self.get_builder().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" WHERE "id" = ? '
#             'OR EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             builder.to_sql()
#         )
#         self.assertEqual([1], builder.get_bindings())
# 
#         builder = self.get_builder()
#         builder.select('*').from_('orders').where('id', '=', 1).or_where_not_exists(
#             self.get_builder().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" WHERE "id" = ? '
#             'OR NOT EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             builder.to_sql()
#         )
#         self.assertEqual([1], builder.get_bindings())
#
#     def test_basic_select_use_write_connection(self):
#         builder = self.get_builder()
#         builder.use_write_connection().select('*').from_('users').get()
#         builder.get_connection().select.assert_called_once_with(
#             'SELECT * FROM "users"',
#             [],
#             False
#         )
#  
#         builder = self.get_builder()
#         builder.select('*').from_('users').get()
#         builder.get_connection().select.assert_called_once_with(
#             'SELECT * FROM "users"',
#             [],
#             True
#         )

if __name__ == "__main__":
    unittest.main()