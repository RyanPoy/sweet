#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
import unittest


class ActiveRecordLimitTest(unittest.TestCase):

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

    def test_limit(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        us = User.limit(3).all
        self.assertEqual(3, len(us))
        self.assertEqual('abc', us[0].username)
        self.assertEqual('efg', us[1].username)
        self.assertEqual('hij', us[2].username)

    def test_limit_and_offset(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        us = User.limit(3, 2).all
        self.assertEqual(3, len(us))
        self.assertEqual('hij', us[0].username)
        self.assertEqual('kmn', us[1].username)
        self.assertEqual('xyz', us[2].username)

    def test_first(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        u = User.first
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertEqual(7, u.age)

    def test_last(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        u = User.last
        self.assertEqual('opq', u.username)
        self.assertEqual('456', u.password)
        self.assertEqual(2, u.age)


if __name__ == '__main__':
    unittest.main()
