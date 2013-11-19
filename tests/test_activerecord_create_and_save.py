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
from pyrails.active_record import ActiveRecord, has_one
from pyrails.active_support import datetime2str
import unittest


class ActiveRecordCreateAndSaveTest(unittest.TestCase):

    def setUp(self):
        drop_table('users')
        create_table("""
create table if not exists users (
    id int auto_increment,
    username varchar(32) not null,
    age int default 0,
    password varchar(32),
    PRIMARY KEY (id)
);""")
        
    def tearDown(self):
        drop_table('users')

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

    def test_has_one_create(self):
        try:
            drop_table('users')
            drop_table('cards')

            create_table("""
    create table if not exists users (
        id int auto_increment,
        name varchar(32) not null,
        PRIMARY KEY (id)
    );
    """)
            create_table("""
    create table if not exists cards (
        id int auto_increment,
        created_at datetime,
        user_id int,
        PRIMARY KEY (id)
    );
    """)
            class Card(ActiveRecord): pass
            class User(ActiveRecord): has_one(Card)

            u = User.create(name='pengyi')
            c = u.create_card(created_at='2012-10-10 12:12:12')
            self.assertIsNotNone(c.id)
            self.assertEqual(u.card, c)

            c2 = Card.find(c.id)
            self.assertEqual(datetime2str(c2.created_at), c.created_at)
        finally:
            drop_table('users')
            drop_table('cards')

if __name__ == '__main__':
    unittest.main()
