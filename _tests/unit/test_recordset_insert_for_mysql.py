#coding: utf8
from sweet._tests import TestCase
from sweet.database.recordset import MySQLRecordset


class TestRecordsetInsertForMySQL(TestCase):

    def get_recordset(self):
        class FakeDB(object): pass
        return MySQLRecordset(db=FakeDB(), tbname="users")

    def test_insert_an_record(self):
        def _(sql, *params):
            self.assertEqual('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', sql)
            self.assertEqual([3, "Poy", 33], list(params))
            return 1
        tb = self.get_recordset()
        tb.db.execute_rowcount = _
        self.assertEqual(1, tb.insert(id=3, name="Poy", age=33))

    def test_insert_getid(self):
        def _(sql, *params):
            self.assertEqual('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', sql)
            self.assertEqual([3, "Poy", 33], list(params))
            return 1
        tb = self.get_recordset()
        tb.db.execute_lastrowid = _
        self.assertEqual(1, tb.insert_getid(id=3, name="Poy", age=33))

    def test_insert_an_record_with_a_dict(self):
        def _(sql, *params):
            self.assertEqual('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s)', sql)
            self.assertEqual([3, "Poy", 33], list(params))
            return 1
        tb = self.get_recordset()
        tb.db.execute_rowcount = _
        self.assertEqual(1, tb.insert(dict(id=3, name="Poy", age=33)))

    def test_insert_multple_records(self):
        def _(sql, *params):
            self.assertEqual('INSERT INTO `users` (`id`, `name`, `age`) VALUES (%s, %s, %s), (%s, %s, %s)', sql)
            self.assertEqual([3, "Poy", 33, 4, "Ryan", 44], list(params))
            return 2
        tb = self.get_recordset()
        tb.db.execute_rowcount = _
        self.assertEqual(
            2, 
            tb.insert([
                dict(id=3, name="Poy", age=33),
                dict(id=4, name="Ryan", age=44),
            ])
        )


if __name__ == '__main__':
    import unittest
    unittest.main()

    