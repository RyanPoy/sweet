#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.record import ActiveRecord
import unittest


class ActiveRecordCreateAndSaveTest(unittest.TestCase):

    def setUp(self):
        self.user_table_name = 'users'
        drop_table(self.user_table_name)
        create_table("""
create table if not exists %s (
    id int auto_increment,
    username varchar(32) not null,
    age int default 0,
    password varchar(32),
    PRIMARY KEY (id)
);""" % self.user_table_name)
        
    def tearDown(self):
        drop_table(self.user_table_name)

    def test_create(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertTrue(u.id is not None)

    def test_should_find_record_after_create(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        u = User.find(1)
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertEqual(1, u.id)

    def test_save(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertTrue(u.id is not None)

    def test_should_find_record_after_save(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        u = User.find(1)
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertEqual(1, u.id)


if __name__ == '__main__':
    unittest.main()
