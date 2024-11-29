#coding: utf8
import unittest
from unittest import mock
from sweet.database import SQLite
from sweet.database import SQLiteRecordset


class TestSQLiteRecordsetDelete(unittest.TestCase):

    def get_db(self):
        class FakeDB(mock.MagicMock):
            qutotation = '`'
            paramstyle = '?'
        FakeDB.aqm = SQLite.aqm
        return FakeDB()

    def test_delete(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)
        tb = SQLiteRecordset(db=db, tablename='users')
        tb.where(id=[1, 2, 3], name='Ryan', age__gte=30).delete()
        db.execute_rowcount.assert_called_once_with('DELETE FROM `users` WHERE `id` IN (?, ?, ?) AND `name` = ? AND `age` >= ?', *[1, 2, 3, "Ryan", 30])

    def test_delete_with_join(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=3)

        tb = SQLiteRecordset(db=db, tablename='users')
        tb = tb.where(id=[1,2,3]).or_where(name="Ryan").join('cars', on='users.id=cars.user_id').delete()
        db.execute_rowcount.assert_called_once_with('DELETE FROM `users` WHERE `users`.`id` IN (SELECT `users`.`id` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (?, ?, ?) OR `name` = ?)', *[1, 2, 3, "Ryan"])

    def test_truncate_and_not_update_sqlite_sequence(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(side_effect=[10, -1])
        tb = SQLiteRecordset(db=db, tablename='users')
        tb.truncate()

        calls = [
            mock.call('DELETE FROM `users`'),
            mock.call("SELECT `name` FROM `sqlite_master` WHERE `type` = 'table' and `name` = 'sqlite_sequence'")
        ]
        db.execute_rowcount.assert_has_calls(calls)

    def test_truncate_and_update_sqlite_sequence(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(side_effect=[10, 1])
        db.execute = mock.MagicMock()
        tb = SQLiteRecordset(db=db, tablename='users')
        tb.truncate()

        calls = [
            mock.call('DELETE FROM `users`'),
            mock.call("SELECT `name` FROM `sqlite_master` WHERE `type` = 'table' and `name` = 'sqlite_sequence'")
        ]
        db.execute_rowcount.assert_has_calls(calls)

        calls = [
            mock.call('DELETE FROM `users`'),
            mock.call("SELECT `name` FROM `sqlite_master` WHERE `type` = 'table' and `name` = 'sqlite_sequence'")
        ]
        db.execute_rowcount.assert_has_calls(calls)
        db.execute.assert_called_once_with('UPDATE sqlite_sequence SET seq = 0 where name = `users`')


    def test_delete_after_find_all(self):
        db = self.get_db()
        db.fetchall = mock.MagicMock()
        db.execute_rowcount = mock.MagicMock()

        tb = SQLiteRecordset(db=db, tablename='users')
        tb = tb.where(id=1, name='Ryan')
        tb.all()
        tb.delete()

        db.fetchall.assert_called_once_with('SELECT * FROM `users` WHERE `id` = ? AND `name` = ?', *[1, 'Ryan'])
        db.execute_rowcount.assert_called_once_with('DELETE FROM `users` WHERE `id` = ? AND `name` = ?', *[1, 'Ryan'])

if __name__ == '__main__':
    unittest.main()

