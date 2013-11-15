#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
from pyrails.active_support import RecordNotFound
import unittest


class ActiveRecordFindTest(unittest.TestCase):

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

    def test_find_one(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")
        u = User.find(1)
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)

    def test_find_some(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")

        us = User.find(1, 2)
        self.assertEqual(2, len(us))

        self.assertEqual('abc', us[0].username)
        self.assertEqual('123', us[0].password)

        self.assertEqual('efg', us[1].username)
        self.assertEqual('456', us[1].password)

    def test_should_throw_exception_if_can_not_found(self):
        class User(ActiveRecord): pass
        self.assertRaises(RecordNotFound, User.find, 1)
        
        User.create(username="abc", password="123")
        self.assertRaises(RecordNotFound, User.find, 1, 2)

    def test_find_method_missing(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")

        u = User.find_by_username_and_password('efg', '456')
        self.assertEqual('efg', u.username)
        self.assertEqual('456', u.password)


if __name__ == '__main__':
    unittest.main()
