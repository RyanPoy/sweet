# -*- coding:utf-8 -*-
from sweet.query import Criteria
import unittest
import fudge


class CriteriaUpdateTestCase(unittest.TestCase):
    
    def get_criteria(self, conn=None):
        return Criteria(conn)

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

if __name__ == "__main__":
    unittest.main()
