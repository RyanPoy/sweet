#coding: utf8
import unittest
from unittest import mock
from sweet.database import MySQL
from sweet.database import MySQLRecordset


class TestMySQLRecordsetInsert(unittest.TestCase):

    def get_db(self):
        class FakeDB(mock.MagicMock):
            qutotation = '`'
            paramstyle = '%s'
        FakeDB.aqm = MySQL.aqm
        return FakeDB()


    def test_insert_an_record(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=1)

        tb = MySQLRecordset(db=db, tablename="users")
        tb.insert(id=3, name="Poy", age=33)
        db.execute_rowcount.assert_called_once_with('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', *[3, "Poy", 33])

    def test_insert_getid(self):
        db = self.get_db()
        db.execute_lastrowid = mock.MagicMock(return_value=1)
        
        tb = MySQLRecordset(db=db, tablename="users")
        tb.insert_getid(id=3, name="Poy", age=33)
        db.execute_lastrowid.assert_called_once_with('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', *[3, "Poy", 33])

    def test_insert_an_record_with_a_dict(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=1)
        
        tb = MySQLRecordset(db=db, tablename="users")
        tb.insert(dict(id=3, name="Poy", age=33))
        db.execute_rowcount.assert_called_once_with('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', *[3, "Poy", 33])

    def test_insert_multple_records(self):
        db = self.get_db()
        db.execute_rowcount = mock.MagicMock(return_value=2)
        
        tb = MySQLRecordset(db=db, tablename="users")
        tb.insert([
            dict(id=3, name="Poy", age=33),
            dict(id=4, name="Ryan", age=44),
        ])
        db.execute_rowcount.assert_called_once_with('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s), (%s, %s, %s)', *[3, "Poy", 33, 4, "Ryan", 44])


if __name__ == '__main__':
    unittest.main()

    