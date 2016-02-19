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

    def test_sub_selects(self):
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(tag='foo').where('age < ', 
            criteria.new_instance().from_('students').select('score').where('score > ?', 10)
        )
        sql, params = criteria.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `users`.`tag` = ? AND age < '
                ' (SELECT `students`.`score` FROM `students` WHERE score > ?)',
            sql
        )
        self.assertEqual(['foo', 10], params)
        
        criteria = self.get_criteria()
        criteria.select('*').from_('users').where(tag='foo').where(age= 
            criteria.new_instance().from_('students').select('score').where('score > ?', 10)
        )
        sql, params = criteria.to_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `users`.`tag` = ? AND `users`.`age` = '
                '(SELECT `students`.`score` FROM `students` WHERE score > ?)',
            sql
        )
        self.assertEqual(['foo', 10], params)
        

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


if __name__ == "__main__":
    unittest.main()
