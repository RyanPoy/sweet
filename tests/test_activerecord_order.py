#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
import unittest


class ActiveRecordOrderTest(unittest.TestCase):

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

    def test_order_asc(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        us = User.order('age').all
        self.assertEqual(6, len(us))
        self.assertEqual('xyz', us[0].username)
        self.assertEqual('opq', us[1].username)
        self.assertEqual('hij', us[2].username)
        self.assertEqual('efg', us[3].username)
        self.assertEqual('kmn', us[4].username)
        self.assertEqual('abc', us[5].username)

    def test_order_desc(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123", age=7)
        User.create(username="efg", password="456", age=5)
        User.create(username="hij", password="123", age=3)
        User.create(username="kmn", password="456", age=6)
        User.create(username="xyz", password="123", age=1)
        User.create(username="opq", password="456", age=2)

        us = User.order('age DESC').all
        self.assertEqual(6, len(us))
        self.assertEqual('abc', us[0].username)
        self.assertEqual('kmn', us[1].username)
        self.assertEqual('efg', us[2].username)
        self.assertEqual('hij', us[3].username)
        self.assertEqual('opq', us[4].username)
        self.assertEqual('xyz', us[5].username)



if __name__ == '__main__':
    unittest.main()
