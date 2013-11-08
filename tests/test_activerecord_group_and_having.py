#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.record import ActiveRecord
from pyrails.exceptions import RecordNotFound
import unittest


class ActiveRecordGroupAndHavingTest(unittest.TestCase):

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

    def test_group_and_having(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=1)
        User.create(username="abc", password="456", age=1)
        User.create(username="efg", password="123", age=1)
        User.create(username="efg", password="456", age=1)
        User.create(username="hij", password="123", age=1)
        User.create(username="hij", password="456", age=1)


        us = User.where(age='1').group('username').having(password="123").all()
        self.assertEqual(3, len(us))
        self.assertEqual('abc', us[0].username)
        self.assertEqual('123', us[0].password)

        self.assertEqual('efg', us[1].username)
        self.assertEqual('123', us[1].password)

        self.assertEqual('hij', us[2].username)
        self.assertEqual('123', us[2].password)


if __name__ == '__main__':
    unittest.main()
