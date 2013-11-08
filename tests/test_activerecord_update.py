#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.record import ActiveRecord
import unittest


class ActiveRecordUpdateTest(unittest.TestCase):

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

    def test_update_attributes(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertTrue(u.update_attributes(username="efg", password="456"))
        self.assertEqual('efg', u.username)
        self.assertEqual('456', u.password)

    def test_should_find_record_after_update_attributes(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertTrue(u.update_attributes(username="efg", password="456"))
        u = User.find(1)
        self.assertEqual('efg', u.username)
        self.assertEqual('456', u.password)

    def test_update_all(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")
        self.assertEqual(True, User.update_all(username="xyz", password="000"))
        us = User.find(1, 2)
        self.assertEqual('xyz', us[0].username)
        self.assertEqual('000', us[0].password)
        self.assertEqual('xyz', us[1].username)
        self.assertEqual('000', us[1].password)        


if __name__ == '__main__':
    unittest.main()
