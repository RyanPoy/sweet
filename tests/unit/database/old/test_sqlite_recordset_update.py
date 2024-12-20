#coding: utf8
import unittest
from sweet.database import SQLite
from sweet.database import SQLiteRecordset
from unittest import mock


class TestSQLiteRecordsetUpdate(unittest.TestCase):

    def get_db(self):
        class FakeDB(mock.MagicMock):
            qutotation = '`'
            paramstyle = '?'
        FakeDB.aqm = SQLite.aqm
        return FakeDB()

    def test_update(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = SQLiteRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.update()
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = ?, `name` = ? WHERE `age` >= ? OR `name` = ?', *[20, 'nothing', 40, 'Ryan'])

    def test_update_with_join(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = SQLiteRecordset(db=db, tablename='users').where(users__id=[1,2,3]).or_where(users__name="Ryan").join('cars', "users.id=cars.user_id")
        tb.update()
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `name` = ? WHERE `users`.`id` IN (SELECT `users`.`id` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `users`.`id` IN (?, ?, ?) OR `users`.`name` = ?)', *['nothing', 1, 2, 3, 'Ryan'])

    def test_increase(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = SQLiteRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.increase(age=10, score=20)
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = `age` + ?, `score` = `score` + ? WHERE `age` >= ? OR `name` = ?', *[10, 20, 40, 'Ryan'])

    def test_decrease(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = SQLiteRecordset(db=db, tablename='users').where(age__gte=40).or_where(name="Ryan")
        tb.decrease(age=10, score=20)
        db.execute_rowcount.assert_called_once_with('UPDATE `users` SET `age` = `age` - ?, `score` = `score` - ? WHERE `age` >= ? OR `name` = ?', *[10, 20, 40, 'Ryan'])


if __name__ == '__main__':
    unittest.main()

