#coding: utf8
import unittest
from unittest import mock
from sweet.database import MySQL
from sweet.database import MySQLRecordset


class TestMySQLRecordsetDelete(unittest.TestCase):

    def get_db(self):
        class FakeDB(mock.MagicMock):
            qutotation = '`'
            paramstyle = '%s'
        FakeDB.aqm = MySQL.aqm
        return FakeDB()

    def test_delete(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users')
        tb.where(id=[1, 2, 3], name='Ryan', age__gte=30).delete()
        db.execute_rowcount.assert_called_once_with('DELETE FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s AND `age` >= %s', *[1, 2, 3, "Ryan", 30])

    def test_delete_with_join(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = MySQLRecordset(db=db, tablename='users')
        tb.where(id=[1,2,3]).or_where(name="Ryan").join('cars', on='users.id=cars.user_id').delete()
        db.execute_rowcount.assert_called_once_with('DELETE `users` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', *[1, 2, 3, 'Ryan'])

    def test_truncate(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=10)
        tb = MySQLRecordset(db=db, tablename='users')
        tb.where(id=[1, 2, 3]).truncate()
        db.execute_rowcount.assert_called_once_with('TRUNCATE `users`')

    def test_delete_after_find_all(self):
        db = self.get_db()
        db.fetchall = mock.MagicMock()
        db.execute_rowcount = mock.MagicMock()

        tb = MySQLRecordset(db=db, tablename='users')
        tb = tb.where(id=1, name='Ryan')
        tb.all()
        tb.delete()

        db.fetchall.assert_called_once_with('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', *[1, 'Ryan'])
        db.execute_rowcount.assert_called_once_with('DELETE FROM `users` WHERE `id` = %s AND `name` = %s', *[1, 'Ryan'])


if __name__ == '__main__':
    unittest.main()

