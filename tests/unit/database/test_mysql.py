#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.table import MySQLTable
from sweet.database import MySQL, Record
from sweet.utils import Collection


class MySQLTest(TestCase):
    
    def setUp(self):
        self.db = MySQL('sweet_test')
        self.remove_record()
        self.prepare_record()

    def tearDown(self):
        self.remove_record()

    def prepare_record(self):
        self.db.execute("insert into users (id, name, age) values (1, 'jack', 25) ")
        self.db.execute("insert into users (id, name, age) values (2, 'lucy', 22) ")

    def remove_record(self):
        self.db.execute("delete from users")

    def test_table(self):
        table = self.db.table('users')
        self.assertTrue(isinstance(table, MySQLTable))

    def test_first(self):
        r = self.db.table('users').first()
        self.assertTrue(isinstance(r, Record))
        self.assertEqual(1, r.id)
        self.assertEqual('jack', r.name)
        self.assertEqual(25, r.age)

    def test_last(self):
        r = self.db.table('users').last()
        self.assertTrue(isinstance(r, Record))
        self.assertEqual(2, r.id)
        self.assertEqual('lucy', r.name)
        self.assertEqual(22, r.age)

    def test_all(self):
        coll = self.db.table('users').all()
        self.assertTrue(isinstance(coll, Collection))
        self.assertEqual(2, len(coll))
        
        self.assertEqual(1, coll[0].id)
        self.assertEqual('jack', coll[0].name)
        self.assertEqual(25, coll[0].age)

        self.assertEqual(2, coll[1].id)
        self.assertEqual('lucy', coll[1].name)
        self.assertEqual(22, coll[1].age)

    def test_where(self):
        coll = self.db.table('users').where(age=22).all()
        self.assertEqual(1, len(coll))
        r = coll[0]
        self.assertEqual(2, r.id)
        self.assertEqual('lucy', r.name)
        self.assertEqual(22, r.age)

    def test_insert_getid(self):
        tb = self.db.table('users')
        user_id = tb.insert_getid(id=3, name="Poy", age=33)
        self.assertEqual(3, user_id)

        rs = tb.where(id=3).all()
        self.assertEqual(1, len(rs))

        r = rs[0]
        self.assertEqual(3, r.id)
        self.assertEqual('Poy', r.name)
        self.assertEqual(33, r.age)

    def test_insert_getid_with_autoincrementid(self):
        tb = self.db.table('users')
        user_id = tb.insert_getid(name="Poy", age=33)

        rs = tb.where(name="Poy").where(age=33).all()
        self.assertEqual(1, len(rs))

        r = rs[0]
        self.assertEqual(user_id, r.id)
        self.assertEqual('Poy', r.name)
        self.assertEqual(33, r.age)

    def test_insert(self):
        tb = self.db.table('users')
        cnt = tb.insert(id=3, name="Poy", age=33)
        self.assertEqual(1, cnt)
        rs = tb.where(id=3).all()
        self.assertEqual(1, len(rs))

        r = rs[0]
        self.assertEqual(3, r.id)
        self.assertEqual('Poy', r.name)
        self.assertEqual(33, r.age)

    def test_multple_insert(self):
        tb = self.db.table('users')
        tb.insert([
            dict(id=3, name="Poy", age=33),
            dict(id=4, name="Ryan", age=44),
        ])
        rs = tb.where(age__gt=30).all()
        self.assertEqual(2, len(rs))

        self.assertEqual(3, rs[0].id)
        self.assertEqual('Poy', rs[0].name)
        self.assertEqual(33, rs[0].age)

        self.assertEqual(4, rs[1].id)
        self.assertEqual('Ryan', rs[1].name)
        self.assertEqual(44, rs[1].age)



if __name__ == '__main__':
    import unittest
    unitest.testmain()
