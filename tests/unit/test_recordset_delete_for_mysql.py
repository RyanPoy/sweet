#coding: utf8
from sweet.tests import TestCase
from sweet.database.recordset import MySQLRecordset


class TestRecordsetDeleteForMysql(TestCase):

    def get_recordset(self):
        class FakeDB(object): pass
        return MySQLRecordset(db=FakeDB(), tbname="users")

#     def test_delete(self):
#         def _(sql, *params):
#             self.assertEqual('DELETE FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s AND `age` >= %s', sql)
#             self.assertEqual([1, 2, 3, "ryanpoy", 30], list(params))
#             return 3
#         tb = self.get_recordset()
#         tb.db.execute_rowcount = _
#         
#         r = tb.where(id=[1, 2, 3], name='ryanpoy', age__gte=30).delete()
#         self.assertEqual(3, r)
# 
#     def test_delete_with_join(self):
#         def _(sql, *params):
#             self.assertEqual('DELETE `users` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
#             self.assertEqual([1, 2, 3, 'ryanpoy'], list(params))
#             return 3
#         tb = self.get_recordset()
#         tb.db.execute_rowcount = _
# 
#         r = tb.where(id=[1,2,3]).or_where(name="ryanpoy").join('cars', on='users.id=cars.user_id').delete()
#         self.assertEqual(3, r)
# 
#     def test_truncate(self):
#         def _(sql, *params):
#             self.assertEqual('TRUNCATE `users`', sql)
#             self.assertTrue(not params)
#             return 10
#         tb = self.get_recordset()
#         tb.db.execute_rowcount = _
# 
#         r = tb.where(id=[1, 2, 3]).truncate()
#         self.assertEqual(10, r)

    def test_delete_after_find_all(self):

        def fetchall(sql, *params):
            self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', sql)
            self.assertEqual([1, 'ryanpoy'], list(params))

        def execute_rowcount(sql, *params):
            self.assertEqual('DELETE FROM `users` WHERE `id` = %s AND `name` = %s', sql)
            self.assertEqual([1, 'ryanpoy'], list(params))

        tb = self.get_recordset()
        tb.db.fetchall = fetchall        
        tb.db.execute_rowcount = execute_rowcount
        
        tb = tb.where(id=1, name='ryanpoy')
        tb.all()
        tb.delete()


if __name__ == '__main__':
    import unittest
    unittest.main()

