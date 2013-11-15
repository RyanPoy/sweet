#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
import unittest


class ActiveRecordWhereTest(unittest.TestCase):

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

    def test_where_with_sql(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")
        us = User.where('username="abc"').where('password="123"').all
        self.assertEqual(1, len(us))

        self.assertEqual('abc', us[0].username)
        self.assertEqual('123', us[0].password)

    def test_where_with_sql_and_params(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")

        us = User.where('username=?', 'efg').all
        self.assertEqual(1, len(us))

        self.assertEqual('efg', us[0].username)
        self.assertEqual('456', us[0].password)

    def test_where_with_dict(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="efg", password="456")

        us = User.where(username='efg').where(password="456").all
        self.assertEqual(1, len(us))
        self.assertEqual('efg', us[0].username)
        self.assertEqual('456', us[0].password)

        us = User.where(username='abc', password="123").all
        self.assertEqual(1, len(us))
        self.assertEqual('abc', us[0].username)
        self.assertEqual('123', us[0].password)

    # def test_where_when_like_query(self):
    #     class User(ActiveRecord): pass
    #     User.create(username="abc", password="123")
    #     User.create(username="efg", password="456")
    #     us = User.where('username like "e?g"', '?').all
    #     self.assertEqual(1, len(us))
    #     self.assertEqual('efg', us[0].username)
    #     self.assertEqual('456', us[0].password)

    def test_where_with_complex_coditions(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        User.create(username="abc", password="456")
        User.create(username="efg", password="123")
        User.create(username="efg", password="456")

        us = User.where(username='efg').where('password=?', '123').all
        self.assertEqual(1, len(us))
        self.assertEqual('efg', us[0].username)
        self.assertEqual('123', us[0].password)


if __name__ == '__main__':
    unittest.main()
