#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.table import MySQLTable


class MySQLTableUpdateTest(TestCase):

    def get_table(self):
        class FakeDB(object): pass
        return MySQLTable(db=FakeDB(), tbname="users")

    def test_update(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = %s, `name` = %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([20, 'nothing', 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.update(age=20, name='nothing'))

    def test_increase(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = `age` + %s, `score` = `score` + %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([10, 20, 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.increase(age=10, score=20))

    def test_decrease(self):
        def _(sql, *params):
            self.assertEqual('UPDATE `users` SET `age` = `age` - %s, `score` = `score` - %s WHERE `age` >= %s OR `name` = %s', sql)
            self.assertEqual([10, 20, 40, 'ryanpoy'], list(params))
            return 3
        tb = self.get_table().where(age__gte=40).or_(name="ryanpoy")
        tb.db.execute_rowcount = _
        self.assertEqual(3, tb.decrease(age=10, score=20))


if __name__ == '__main__':
    import unittest
    unittest.main()

