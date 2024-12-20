#coding: utf8
import unittest
from sweet.database import MySQL
from sweet.database import MySQLRecordset
from unittest import mock


class TestMySQLRecordsetUpdate(unittest.TestCase):

    def get_db(self):
        class FakeDB(mock.MagicMock):
            qutotation = '`'
            paramstyle = '%s'
        FakeDB.aqm = MySQL.aqm
        return FakeDB()

    def test_update(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.update()
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = %s, `name` = %s WHERE `age` >= %s OR `name` = %s', *[20, 'nothing', 40, 'Ryan'])

    def test_update_with_join(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users').where(id=[1,2,3]).or_where(name="Ryan").join('cars', "users.id=cars.user_id")
        tb.update()
        db.execute_rowcount.assert_called_once_with('UPDATE `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` SET `name` = %s WHERE `id` IN (%s, %s, %s) OR `name` = %s', *['nothing', 1, 2, 3, 'Ryan'])

    def test_increase(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.increase(age=10, score=20)
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = `age` + %s, `score` = `score` + %s WHERE `age` >= %s OR `name` = %s', *[10, 20, 40, 'Ryan'])

    def test_decrease(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.decrease(age=10, score=20)
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = `age` - %s, `score` = `score` - %s WHERE `age` >= %s OR `name` = %s', *[10, 20, 40, 'Ryan'])


if __name__ == '__main__':
    unittest.main()

