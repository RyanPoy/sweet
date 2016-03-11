# -*- coding:utf-8 -*-
from sweet.criteria import Criteria
import unittest
import fudge


class CriteriaInsertTestCase(unittest.TestCase):

    def get_criteria(self, db=None):
        return Criteria(db)
    
    def test_insert(self):
        db = fudge.Fake('db')\
                .expects('execute_lastrowid')\
                .with_args('INSERT INTO `users` (`age`, `id`, `name`) VALUES (?, ?, ?)', 'boom', 'foo', 'bar')\
                .returns(199)
        criteria = self.get_criteria(db)
        relt = criteria.from_('users').insert(id='foo', name='bar', age='boom')
        self.assertEqual(199, relt)

    def test_mysql_batch_insert(self):
        db = fudge.Fake('db')\
                .expects('execute_rowcount')\
                .with_args('INSERT INTO `users` (`age`, `name`, `id`) VALUES (?, ?, ?), (?, ?, ?)', 'boom', 'bar', 'foo', 'boom2', 'bar2', 'foo2')\
                .returns(2)\

        criteria = self.get_criteria(db)
        relt = criteria.from_('users').insert([ 
            dict(id='foo', name='bar', age='boom'), 
            dict(id='foo2', name='bar2', age='boom2'),
        ])
        self.assertEqual(2, relt)

#     def test_sqlite_batch_insert(self):
#         criteria = self.get_sqlite_builder()
#         query = 'INSERT INTO "users" ("email", "name") ' \
#                 'SELECT ? AS "email", ? AS "name" UNION ALL SELECT ? AS "email", ? AS "name"'
#         criteria.get_connection().insert.return_value = True
#         result = criteria.from_('users').insert([
#             {'email': 'foo', 'name': 'py'},
#             {'email': 'bar', 'name': 'ryan'}
#         ])
#         criteria.get_connection().insert.assert_called_once_with(
#             query, ['foo', 'py', 'bar', 'ryan']
#         )
#         self.assertTrue(result)
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

 
if __name__ == "__main__":
    unittest.main()
