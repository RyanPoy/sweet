#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.table import MySQLTable


class MySQLTableDeleteTest(TestCase):

    def get_table(self):
        class FakeDB(object): pass
        return MySQLTable(db=FakeDB(), tbname="users")

    def test_delete(self):
        def _(sql, *params):
            self.assertEqual('DELETE FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s AND `age` >= %s', sql)
            self.assertEqual([1, 2, 3, "ryanpoy", 30], list(params))
            return 3
        tb = self.get_table()
        tb.db.execute_rowcount = _
        
        r = tb.where(id=[1, 2, 3], name='ryanpoy', age__gte=30).delete()
        self.assertEqual(3, r)

    def test_delete_with_join(self):
        def _(sql, *params):
            self.assertEqual('DELETE `users` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `id` IN (%s, %s, %s) OR `name` = %s', sql)
            self.assertEqual([1, 2, 3, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table()
        tb.db.execute_rowcount = _

        r = tb.where(id=[1,2,3]).or_(name="ryanpoy").join('cars', on="users.id=cars.user_id").delete()
        self.assertEqual(3, r)

    def test_truncate(self):
        def _(sql, *params):
            self.assertEqual('TRUNCATE `users`', sql)
            self.assertTrue(not params)
            return 10
        tb = self.get_table()
        tb.db.execute_rowcount = _

        r = tb.where(id=[1, 2, 3]).truncate()
        self.assertEqual(10, r)

if __name__ == '__main__':
    import unittest
    unitest.testmain()

