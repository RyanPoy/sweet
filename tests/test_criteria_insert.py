# -*- coding:utf-8 -*-
from sweet.criteria import Criteria
import unittest
import fudge


class CriteriaInsertTestCase(unittest.TestCase):

    def get_criteria(self, conn=None):
        return Criteria(conn)
    
    def test_insert(self):
        lastrowid=199
        conn = fudge.Fake('conn').has_attr(_cursor=fudge.Fake('cursor').has_attr(lastrowid=lastrowid))\
                .expects('execute')\
                .with_args('INSERT INTO `users` (`age`, `id`, `name`) VALUES (?, ?, ?)', 'boom', 'foo', 'bar')
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').insert(id='foo', name='bar', age='boom')
        self.assertEqual(lastrowid, relt)

    def test_mysql_batch_insert(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('INSERT INTO `users` (`age`, `name`, `id`) VALUES (?, ?, ?), (?, ?, ?)', 'boom', 'bar', 'foo', 'boom2', 'bar2', 'foo2')\
                .returns(True)\

        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').insert([ 
            dict(id='foo', name='bar', age='boom'), 
            dict(id='foo2', name='bar2', age='boom2'),
        ])
        self.assertTrue(relt)

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
