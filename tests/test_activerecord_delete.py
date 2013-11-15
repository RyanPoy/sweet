#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
import unittest


class ActiveRecordDeleteTest(unittest.TestCase):

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

    def test_delete(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertEqual(True, u.delete())

    def test_delete_all(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")
        self.assertEqual(True, User.delete_all())


if __name__ == '__main__':
    unittest.main()
