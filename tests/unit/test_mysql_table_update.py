#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.recordset import MySQLRecordset


class MySQLRecordsetUpdateTest(TestCase):

    def get_table(self):
        class FakeDB(object): pass
        return MySQLRecordset(db=FakeDB(), tbname="users")

    def test_update(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = %s, `name` = %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([20, 'nothing', 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_where(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.update(age=20, name='nothing'))

    def test_update_with_join(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` SET `name` = %s WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
            self.assertEqual([ 'nothing', 1, 2, 3, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table()
        tb.db.execute_rowcount = _

        r = tb.where(id=[1,2,3]).or_where(name="ryanpoy").join('cars', "users.id=cars.user_id").update(name='nothing')
        self.assertEqual(3, r)

    def test_increase(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = `age` + %s, `score` = `score` + %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([10, 20, 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_where(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.increase(age=10, score=20))

    def test_decrease(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = `age` - %s, `score` = `score` - %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([10, 20, 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_where(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.decrease(age=10, score=20))


if __name__ == '__main__':
    import unittest
    unittest.main()

