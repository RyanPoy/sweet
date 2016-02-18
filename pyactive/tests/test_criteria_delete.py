# -*- coding:utf-8 -*-

from pyactive.query.criteria import Criteria
import unittest
import fudge


class CriteriaDeleteTestCase(unittest.TestCase):

    def get_criteria(self, conn=None):
        return Criteria(conn)
    
    def test_basic_delete(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('DELETE FROM `users`')\
                .returns(1)
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').delete()
        self.assertEqual(1, relt)

    def test_delete_with_where(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('DELETE FROM `users` WHERE `users`.`email` = ? AND `users`.`name` = ?', 'foo', 'bar')\
                .returns(1)
        criteria = self.get_criteria(conn)
        relt = criteria.from_('users').where(email='foo').where(name='bar').delete()
        self.assertEqual(1, relt)
        
    def test_delete_with_join(self):
        conn = fudge.Fake('conn')\
                .expects('execute')\
                .with_args('DELETE FROM `users` '
                           'INNER JOIN `contacts` ON users.id = contacts.id '
                           'WHERE `users`.`email` = ? AND `users`.`name` = ?', 'foo', 'bar')\
                .returns(1)
        criteria = self.get_criteria(conn)
        criteria.from_('users').where(email='foo').where(name='bar').join('contacts', 'users.id = contacts.id')
        relt = criteria.delete()
        self.assertEqual(1, relt)
 
if __name__ == "__main__":
    unittest.main()
