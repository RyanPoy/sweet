#coding: utf8
from pyactive.query.criteria import Criteria
from pyactive.query.join_clause import JoinClause
import unittest
import fudge


class CriteriaQueryTestCase(unittest.TestCase):
    
    def get_criteria(self, conn=None):
        return Criteria(conn)

    def test_basic_select(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users')
        
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)

    def test_alias_select(self):
        criteria = self.get_criteria()
        criteria.select('foo as bar').from_('users')
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT `foo` AS `bar` FROM `users`', sql )
        self.assertEqual([], params)

        criteria = self.get_criteria()
        criteria.select('x.y AS foo.bar').from_('baz')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT `x`.`y` AS `foo`.`bar` FROM `baz`', sql )
        self.assertEqual([], params)

    def test_adding_selects(self):
        criteria = self.get_criteria()
        criteria.select('foo').select('bar').select('baz', 'boom').select('x.y AS foo.bar').from_('users')
        
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT `users`.`foo`, `users`.`bar`, `users`.`baz`, `users`.`boom`, `x`.`y` AS `foo`.`bar` FROM `users`', sql )
        self.assertEqual([], params)

    def test_basic_select_distinct(self):
        criteria = self.get_criteria()
        criteria.distinct().select('foo', 'bar').from_('users')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT DISTINCT `users`.`foo`, `users`.`bar` FROM `users`', sql )
        self.assertEqual([], params) 

    def test_basic_where(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=1)
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ?', sql )
        self.assertEqual([1], params)
        
    def test_where_in(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=[1, 2])
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` IN (?, ?)', sql )
        self.assertEqual([1, 2], params)
        
    def test_raw_where(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE id = ? or email = ?', sql )
        self.assertEqual([1, 'foo'], params)
         
    def test_multi_contidions_where(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=1).where('email in (?, ?)', 'foo', 'bar')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ? AND email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)

    def test_empty_where_in(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=[])
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)
 
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=[]).where('email in (?, ?)', 'foo', 'bar')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE email in (?, ?)', sql )
        self.assertEqual(['foo', 'bar'], params)
        
    def test_basic_where_null(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=None)
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `users`.`id` IS NULL', sql)
        self.assertEqual([], params)

    def test_group_by(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').group_by('id', 'email').group_by('name')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` GROUP BY `users`.`id`, `users`.`email`, `users`.`name`', sql )
        self.assertEqual( [], params )

        criteria = self.get_criteria()
        criteria.select('*').from_('users').group_by('id, email').group_by('name')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` GROUP BY `users`.`id`, `users`.`email`, `users`.`name`', sql )
        self.assertEqual( [], params )

    def test_order_by(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').order_by('email').order_by('age desc, id ASC')
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` ORDER BY `users`.`email`, `users`.`age` DESC, `users`.`id` ASC', sql)
        self.assertEqual( [], params )

        criteria = self.get_criteria()
        criteria.select('*').from_('users').order_by('email').order_by('age DESC', 'id ASC')
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` ORDER BY `users`.`email`, `users`.`age` DESC, `users`.`id` ASC', sql)
        self.assertEqual( [], params )
 
    def test_basic_having(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=1)
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` = ?', sql )
        self.assertEqual([1], params)
        
    def test_having_in(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=[1, 2])
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` IN (?, ?)', sql )
        self.assertEqual([1, 2], params)
        
    def test_raw_having(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having('id = ? or email = ?', 1, 'foo')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING id = ? or email = ?', sql )
        self.assertEqual([1, 'foo'], params)

    def test_multi_contidions_having(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=1).having('email in (?, ?)', 'foo', 'bar')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING `users`.`id` = ? AND email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)

    def test_empty_having_in(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=[])
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)
 
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=[]).having('email in (?, ?)', 'foo', 'bar')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` HAVING email in (?, ?)', sql )
        self.assertEqual(['foo', 'bar'], params)
        
    def test_basic_having_null(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').having(id=None)
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `users`.`id` IS NULL', sql)
        self.assertEqual([], params)
        
    def test_multi_contidions_having_follow_group(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(id=1).having('email in (?, ?)', 'foo', 'bar').group_by('name')
        sql, params = criteria.to_sql()
        self.assertEqual( 'SELECT * FROM `users` WHERE `users`.`id` = ? GROUP BY `users`.`name` HAVING email in (?, ?)', sql )
        self.assertEqual([1, 'foo', 'bar'], params)
 
    def test_limits_and_offsets(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').offset(15).limit(10)
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 15', sql)
        self.assertEqual([], params)

    def test_page(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').page(5, 10)
        sql, params = criteria.to_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 50', sql)
        self.assertEqual([], params)

    def test_basic_joins(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users')\
            .join('contacts', 'users.id = contacts.id')\
            .left_join('photos', 'users.id=photos.user_id')\
            .right_join('cards', 'users.id=?', 'cards.user_id')
        sql, params = criteria.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` '
            'INNER JOIN `contacts` ON users.id = contacts.id '
            'LEFT JOIN `photos` ON users.id=photos.user_id '
            'RIGHT JOIN `cards` ON users.id=?',
            sql
        )
        self.assertEqual(['cards.user_id'], params)

    def test_complex_joins(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').join(
            JoinClause('contacts')
            .on('users.id=contacts.id')
            .on('users.name=?', 'contacts.name')
        )
        sql, params = criteria.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` '
            'INNER JOIN `contacts` ON users.id=contacts.id '
            'AND users.name=?',
            sql
        )
        self.assertEqual(['contacts.name'], params)

#     def test_raw_expression_in_select(self):
#         criteria = self.get_criteria()
#         criteria.select(QueryExpression('substr(foo, 6)')).from_('users')
#         self.assertEqual('SELECT substr(foo, 6) FROM "users"', criteria.to_sql())
# 

    def test_first_return_none_result_if_can_not_found(self):
        results = []
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE id = ? or email = ? LIMIT 1', 1, 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        limit, offset = criteria._limit, criteria._offset
        
        criteria.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        self.assertEqual(None, criteria.first() )
        self.assertEqual(limit, criteria._limit)
        self.assertEqual(offset, criteria._offset)
        
    def test_first_return_first_result(self):
        results = [
            {'id': 1, 'email': 'foo'}
        ]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE id = ? or email = ? LIMIT 1', 1, 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        limit, offset = criteria._limit, criteria._offset
        
        criteria.select('*').from_('users').where('id = ? or email = ?', 1, 'foo')
        self.assertEqual(results[0], criteria.first() )
        self.assertEqual(limit, criteria._limit)
        self.assertEqual(offset, criteria._offset)
        
    def test_last_return_last_results(self):
        results = [
            {'id': 20, 'tag': 'foo'},
        ]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT * FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1 OFFSET 19', 'boom', 'foo')\
                .returns(results)
        criteria = Criteria(conn)
        criteria.count = lambda column='*': 20
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(results[0], criteria.last())

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
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(results, criteria.all() )

    def test_update_with_kwargs(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('UPDATE `users` SET `users`.`email` = ?, `users`.`name` = ? WHERE `users`.`id` = ?', 'foo', 'bar', 1)\
                .returns(True)
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').where(id=1).update(email='foo', name='bar')
        self.assertTrue(relt)
 
    def test_update_with_dict(self):
        conn = fudge.Fake('conn')\
                    .expects('execute')\
                    .with_args('UPDATE `users` SET `users`.`name` = ?, `users`.`email` = ? WHERE `users`.`id` = ?', 'bar', 'foo', 1)\
                    .returns(True)
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').where(id=1).update(dict(email='foo', name='bar'))
        self.assertTrue(relt)
    
    def test_update_with_dict_and_kwargs(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('UPDATE `users` SET `users`.`email` = ?, `users`.`name` = ? WHERE `users`.`id` = ?', 'foo', 'bar', 1)\
                .returns(True)
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').where(id=1).update({'email': 'foo'}, name='bar')
        self.assertTrue(relt)
  
    def test_update_with_joins(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('UPDATE `users` INNER JOIN `orders` ON users.id=orders.user_id '
                           'SET `users`.`email` = ?, `users`.`name` = ? WHERE `users`.`id` = ?', 'foo', 'bar', 1)\
                .returns(True)
        criteria = self.get_criteria(conn)
        criteria.from_('users').join('orders', 'users.id=orders.user_id').where(id=1)
        relt = criteria.update({'email': 'foo'}, name='bar')
        self.assertEqual(1, relt)
 
#     def test_update_on_postgres(self):
#         criteria = self.get_postgres_builder()
#         marker = criteria.get_grammar().get_marker()
#         query = 'UPDATE "users" SET "email" = %s, "name" = %s WHERE "id" = %s' % (marker, marker, marker)
#         criteria.get_connection().update.return_value = 1
#         result = criteria.from_('users').where('id', '=', 1).update(email='foo', name='bar')
#         criteria.get_connection().update.assert_called_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_with_joins_on_postgres(self):
#         criteria = self.get_postgres_builder()
#         marker = criteria.get_grammar().get_marker()
#         query = 'UPDATE "users" ' \
#                 'SET "email" = %s, "name" = %s ' \
#                 'FROM "orders" WHERE "id" = %s AND "users"."id" = "orders"."user_id"'\
#                 % (marker, marker, marker)
#         criteria.get_connection().update.return_value = 1
#         result = criteria.from_('users')\
#             .join('orders', 'users.id', '=', 'orders.user_id')\
#             .where('id', '=', 1)\
#             .update(email='foo', name='bar')
#         criteria.get_connection().update.assert_called_once_with(
#             query, ['foo', 'bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_update_respects_raw(self):
#         criteria = self.get_criteria()
#         marker = '?'
#         query = 'UPDATE "users" SET "email" = foo, "name" = %s WHERE "id" = %s' % (marker, marker)
#         criteria.get_connection().update.return_value = 1
#         result = criteria.from_('users').where('id', '=', 1).update(email=QueryExpression('foo'), name='bar')
#         criteria.get_connection().update.assert_called_once_with(
#             query, ['bar', 1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_delete(self):
#         criteria = self.get_criteria()
#         query = 'DELETE FROM "users" WHERE "email" = ?'
#         criteria.get_connection().delete.return_value = 1
#         result = criteria.from_('users').where('email', '=', 'foo').delete()
#         criteria.get_connection().delete.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertEqual(1, result)
# 
#         criteria = self.get_criteria()
#         query = 'DELETE FROM "users" WHERE "id" = ?'
#         criteria.get_connection().delete.return_value = 1
#         result = criteria.from_('users').delete(1)
#         criteria.get_connection().delete.assert_called_once_with(
#             query, [1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_delete_with_join(self):
#         criteria = self.get_mysql_builder()
#         marker = criteria.get_grammar().get_marker()
#         query = 'DELETE `users` FROM `users` ' \
#                 'INNER JOIN `contacts` ON `users`.`id` = `contacts`.`id` WHERE `email` = %s' % marker
#         criteria.get_connection().delete.return_value = 1
#         result = criteria.from_('users')\
#             .join('contacts', 'users.id', '=', 'contacts.id')\
#             .where('email', '=', 'foo')\
#             .delete()
#         criteria.get_connection().delete.assert_called_once_with(
#             query, ['foo']
#         )
#         self.assertEqual(1, result)
# 
#         criteria = self.get_mysql_builder()
#         marker = criteria.get_grammar().get_marker()
#         query = 'DELETE `users` FROM `users` ' \
#                 'INNER JOIN `contacts` ON `users`.`id` = `contacts`.`id` WHERE `id` = %s' % marker
#         criteria.get_connection().delete.return_value = 1
#         result = criteria.from_('users')\
#             .join('contacts', 'users.id', '=', 'contacts.id')\
#             .delete(1)
#         criteria.get_connection().delete.assert_called_once_with(
#             query, [1]
#         )
#         self.assertEqual(1, result)
# 
#     def test_truncate(self):
#         criteria = self.get_criteria()
#         query = 'TRUNCATE "users"'
#         criteria.from_('users').truncate()
#         criteria.get_connection().statement.assert_called_once_with(
#             query, []
#         )
# 
#         criteria = self.get_sqlite_builder()
#         criteria.from_('users')
#         self.assertEqual({
#             'DELETE FROM sqlite_sequence WHERE name = ?': ['users'],
#             'DELETE FROM "users"': []
#         }, criteria.get_grammar().compile_truncate(builder))
# 
#     def test_postgres_insert_get_id(self):
#         criteria = self.get_postgres_builder()
#         marker = criteria.get_grammar().get_marker()
#         query = 'INSERT INTO "users" ("email") VALUES (%s) RETURNING "id"' % marker
#         criteria.get_processor().process_insert_get_id.return_value = 1
#         result = criteria.from_('users').insert_get_id({'email': 'foo'}, 'id')
#         criteria.get_processor().process_insert_get_id.assert_called_once_with(
#             builder, query, ['foo'], 'id'
#         )
#         self.assertEqual(1, result)
# 
#     def test_mysql_wrapping(self):
#         criteria = self.get_mysql_builder()
#         criteria.select('*').from_('users')
#         self.assertEqual(
#             'SELECT * FROM `users`',
#             criteria.to_sql()
#         )
# 
#     def test_merge_wheres_can_merge_wheres_and_bindings(self):
#         criteria = self.get_criteria()
#         criteria.wheres = ['foo']
#         criteria.merge_wheres(['wheres'], ['foo', 'bar'])
#         self.assertEqual(['foo', 'wheres'], criteria.wheres)
#         self.assertEqual(['foo', 'bar'], criteria.get_bindings())
# 
#     def test_where_with_null_second_parameter(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('foo', None)
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "foo" IS NULL',
#             criteria.to_sql()
#         )
# 
#     def test_dynamic_where(self):
#         method = 'where_foo_bar_and_baz_or_boom'
#         parameters = ['john', 'jane', 'bam']
# 
#         criteria = self.get_criteria()
#         criteria.where = mock.MagicMock(return_value=builder)
# 
#         getattr(builder, method)(*parameters)
# 
#         criteria.where.assert_has_calls([
#             mock.call('foo_bar', '=', parameters[0], 'and'),
#             mock.call('baz', '=', parameters[1], 'and'),
#             mock.call('boom', '=', parameters[2], 'or')
#         ])
# 
#     def test_dynamic_where_is_not_greedy(self):
#         method = 'where_ios_version_and_android_version_or_orientation'
#         parameters = ['6.1', '4.2', 'Vertical']
# 
#         criteria = self.get_criteria()
#         criteria.where = mock.MagicMock(return_value=builder)
# 
#         getattr(builder, method)(*parameters)
# 
#         criteria.where.assert_has_calls([
#             mock.call('ios_version', '=', parameters[0], 'and'),
#             mock.call('android_version', '=', parameters[1], 'and'),
#             mock.call('orientation', '=', parameters[2], 'or')
#         ])
# 
#     def test_call_triggers_dynamic_where(self):
#         criteria = self.get_criteria()
# 
#         self.assertEqual(builder, criteria.where_foo_and_bar('baz', 'boom'))
#         self.assertEqual(2, len(criteria.wheres))
# 
#     def test_builder_raises_exception_with_undefined_method(self):
#         criteria = self.get_criteria()
# 
#         try:
#             criteria.do_not_exist()
#             self.fail('Builder did not raise and AttributeError exception')
#         except AttributeError:
#             self.assertTrue(True)
# 
#     def test_mysql_lock(self):
#         criteria = self.get_mysql_builder()
#         marker = criteria.get_grammar().get_marker()
#         criteria.select('*').from_('foo').where('bar', '=', 'baz').lock()
#         self.assertEqual(
#             'SELECT * FROM `foo` WHERE `bar` = %s FOR UPDATE' % marker,
#             criteria.to_sql()
#         )
#         self.assertEqual(['baz'], criteria.get_bindings())
# 
#         criteria = self.get_mysql_builder()
#         marker = criteria.get_grammar().get_marker()
#         criteria.select('*').from_('foo').where('bar', '=', 'baz').lock(False)
#         self.assertEqual(
#             'SELECT * FROM `foo` WHERE `bar` = %s LOCK IN SHARE MODE' % marker,
#             criteria.to_sql()
#         )
#         self.assertEqual(['baz'], criteria.get_bindings())
# 
#     def test_postgres_lock(self):
#         criteria = self.get_postgres_builder()
#         marker = criteria.get_grammar().get_marker()
#         criteria.select('*').from_('foo').where('bar', '=', 'baz').lock()
#         self.assertEqual(
#             'SELECT * FROM "foo" WHERE "bar" = %s FOR UPDATE' % marker,
#             criteria.to_sql()
#         )
#         self.assertEqual(['baz'], criteria.get_bindings())
# 
#         criteria = self.get_postgres_builder()
#         marker = criteria.get_grammar().get_marker()
#         criteria.select('*').from_('foo').where('bar', '=', 'baz').lock(False)
#         self.assertEqual(
#             'SELECT * FROM "foo" WHERE "bar" = %s FOR SHARE' % marker,
#             criteria.to_sql()
#         )
#         self.assertEqual(['baz'], criteria.get_bindings())
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
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users')\
#             .order_by_raw('match ("foo") against(?)', ['bar'])\
#             .where('registered', True)\
#             .group_by('city')\
#             .having('population', '>', 3)\
#             .join(JoinClause('othertable').where('bar', '=', 'foo'))
# 
#         self.assertEqual(expected_sql, criteria.to_sql())
#         self.assertEqual(expected_bindings, criteria.get_bindings())
# 
#     def test_add_binding_with_list_merges_bindings(self):
#         criteria = self.get_criteria()
#         criteria.add_binding(['foo', 'bar'])
#         criteria.add_binding(['baz'])
#         self.assertEqual(['foo', 'bar', 'baz'], criteria.get_bindings())
# 
#     def test_add_binding_with_list_merges_bindings_in_correct_order(self):
#         criteria = self.get_criteria()
#         criteria.add_binding(['bar', 'baz'], 'having')
#         criteria.add_binding(['foo'], 'where')
#         self.assertEqual(['foo', 'bar', 'baz'], criteria.get_bindings())
# 
#     def test_merge_builders(self):
#         criteria = self.get_criteria()
#         criteria.add_binding('foo', 'where')
#         criteria.add_binding('baz', 'having')
#         other_criteria = self.get_criteria()
#         other_criteria.add_binding('bar', 'where')
#         criteria.merge_bindings(other_builder)
#         self.assertEqual(['foo', 'bar', 'baz'], criteria.get_bindings())
# 
#     def test_sub_select(self):
#         criteria = self.get_criteria()
#         marker = criteria.get_grammar().get_marker()
#         expected_sql = 'SELECT "foo", "bar", (SELECT "baz" FROM "two" WHERE "subkey" = %s) AS "sub" ' \
#                        'FROM "one" WHERE "key" = %s' % (marker, marker)
#         expected_bindings = ['subval', 'val']
# 
#         criteria.from_('one').select('foo', 'bar').where('key', '=', 'val')
#         criteria.select_sub(criteria.new_query().from_('two').select('baz').where('subkey', '=', 'subval'), 'sub')
#         self.assertEqual(expected_sql, criteria.to_sql())
#         self.assertEqual(expected_bindings, criteria.get_bindings())
# 
#     def test_chunk(self):
#         criteria = self.get_criteria()
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
#         criteria.get_connection().select.side_effect = select
# 
#         criteria.get_processor().process_select = mock.MagicMock(side_effect=lambda builder_, results_: results_)
# 
#         i = 0
#         for users in criteria.from_('users').chunk(1):
#             self.assertEqual(users[0], results[i])
# 
#             i += 1
# 
#         criteria = self.get_criteria()
#         results = [
#             {'foo': 'bar'},
#             {'foo': 'baz'},
#             {'foo': 'bam'},
#             {'foo': 'boom'}
#         ]
# 
#         criteria.get_connection().select.side_effect = select
# 
#         criteria.get_processor().process_select = mock.MagicMock(side_effect=lambda builder_, results_: results_)
# 
#         for users in criteria.from_('users').chunk(2):
#             self.assertEqual(2, len(users))
# 
#     def test_not_specifying_columns_sects_all(self):
#         criteria = self.get_criteria()
#         criteria.from_('users')
# 
#         self.assertEqual(
#             'SELECT * FROM "users"',
#             criteria.to_sql()
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
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? UNION SELECT * FROM "users" WHERE "id" = ?',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#         criteria = self.get_mysql_builder()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) UNION (SELECT * FROM `users` WHERE `id` = %s)',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_union_alls(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union_all(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? UNION ALL SELECT * FROM "users" WHERE "id" = ?',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_multiple_unions(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         criteria.union(self.get_criteria().select('*').from_('users').where('id', '=', 3))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ?',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2, 3], criteria.get_bindings())
# 
#     def test_multiple_union_alls(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union_all(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         criteria.union_all(self.get_criteria().select('*').from_('users').where('id', '=', 3))
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION ALL SELECT * FROM "users" WHERE "id" = ? '
#             'UNION ALL SELECT * FROM "users" WHERE "id" = ?',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2, 3], criteria.get_bindings())
# 
#     def test_union_order_bys(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         criteria.order_by('id', 'desc')
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'ORDER BY "id" DESC',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_union_limits_and_offsets(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_criteria().select('*').from_('users').where('id', '=', 2))
#         criteria.skip(5).take(10)
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" = ? '
#             'UNION SELECT * FROM "users" WHERE "id" = ? '
#             'LIMIT 10 OFFSET 5',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_mysql_union_order_bys(self):
#         criteria = self.get_mysql_builder()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         criteria.order_by('id', 'desc')
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) '
#             'UNION (SELECT * FROM `users` WHERE `id` = %s) '
#             'ORDER BY `id` DESC',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_mysql_union_limits_and_offsets(self):
#         criteria = self.get_mysql_builder()
#         criteria.select('*').from_('users').where('id', '=', 1)
#         criteria.union(self.get_mysql_builder().select('*').from_('users').where('id', '=', 2))
#         criteria.skip(5).take(10)
#         self.assertEqual(
#             '(SELECT * FROM `users` WHERE `id` = %s) '
#             'UNION (SELECT * FROM `users` WHERE `id` = %s) '
#             'LIMIT 10 OFFSET 5',
#             criteria.to_sql()
#         )
#         self.assertEqual([1, 2], criteria.get_bindings())
# 
#     def test_sub_select_where_in(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where_in(
#             'id',
#             self.get_criteria().select('id').from_('users').where('age', '>', 25).take(3)
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" IN (SELECT "id" FROM "users" WHERE "age" > ? LIMIT 3)',
#             criteria.to_sql()
#         )
#         self.assertEqual([25], criteria.get_bindings())
# 
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where_not_in(
#             'id',
#             self.get_criteria().select('id').from_('users').where('age', '>', 25).take(3)
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "id" NOT IN (SELECT "id" FROM "users" WHERE "age" > ? LIMIT 3)',
#             criteria.to_sql()
#         )
#         self.assertEqual([25], criteria.get_bindings())
#     def test_nested_wheres(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where(email='foo').where(
#             criteria.new_instance().where(name='bar').where(age=25)
#         )
#         sql, params = criteria.to_sql()
#         self.assertEqual(
#             'SELECT * FROM `users` WHERE `users`.`email` = ? AND (`users`.`name` = ? AND `users`.`age` = ?)',
#             criteria.to_sql()
#         )
#         self.assertEqual(['foo', 'bar', 25], criteria.get_bindings())
#  
#     def test_full_sub_selects(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').where('email', '=', 'foo').or_where(
#             'id', '=', criteria.new_query().select(QueryExpression('max(id)')).from_('users').where('email', '=', 'bar')
#         )
#         self.assertEqual(
#             'SELECT * FROM "users" WHERE "email" = ? OR "id" = (SELECT max(id) FROM "users" WHERE "email" = ?)',
#             criteria.to_sql()
#         )
#         self.assertEqual(['foo', 'bar'], criteria.get_bindings())
# 
#     def test_where_exists(self):
#         criteria = self.get_criteria()
#         criteria.select('*').from_('orders').where_exists(
#             self.get_criteria().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" '
#             'WHERE EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             criteria.to_sql()
#         )
#         self.assertEqual([], criteria.get_bindings())
# 
#         criteria = self.get_criteria()
#         criteria.select('*').from_('orders').where_not_exists(
#             self.get_criteria().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" '
#             'WHERE NOT EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             criteria.to_sql()
#         )
#         self.assertEqual([], criteria.get_bindings())
# 
#         criteria = self.get_criteria()
#         criteria.select('*').from_('orders').where('id', '=', 1).or_where_exists(
#             self.get_criteria().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" WHERE "id" = ? '
#             'OR EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             criteria.to_sql()
#         )
#         self.assertEqual([1], criteria.get_bindings())
# 
#         criteria = self.get_criteria()
#         criteria.select('*').from_('orders').where('id', '=', 1).or_where_not_exists(
#             self.get_criteria().select('*').from_('products').where('products.id', '=', QueryExpression('"orders"."id"'))
#         )
#         self.assertEqual(
#             'SELECT * FROM "orders" WHERE "id" = ? '
#             'OR NOT EXISTS (SELECT * FROM "products" WHERE "products"."id" = "orders"."id")',
#             criteria.to_sql()
#         )
#         self.assertEqual([1], criteria.get_bindings())
#
#     def test_basic_select_use_write_connection(self):
#         criteria = self.get_criteria()
#         criteria.use_write_connection().select('*').from_('users').get()
#         criteria.get_connection().select.assert_called_once_with(
#             'SELECT * FROM "users"',
#             [],
#             False
#         )
#  
#         criteria = self.get_criteria()
#         criteria.select('*').from_('users').get()
#         criteria.get_connection().select.assert_called_once_with(
#             'SELECT * FROM "users"',
#             [],
#             True
#         )

if __name__ == "__main__":
    unittest.main()
