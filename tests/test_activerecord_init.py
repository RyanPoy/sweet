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
from pyrails.active_record import ActiveRecord
from pyrails.active_record import has_one, belongs_to
from pyrails.tests import drop_table, create_table
import unittest


class Card(ActiveRecord): belongs_to('pyrails.tests.test_activerecord_init.User')
class User(ActiveRecord): has_one(Card)


class ActiveRecordInitTest(unittest.TestCase):

    def setUp(self):
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

    def TearDown(self):
        drop_table('users')
        drop_table('cards')

    def test_init(self):
        class User(ActiveRecord): pass
        u = User(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)

    def test_has_one_build(self):
        u = User.create(name='pengyi')
        c = u.build_card(created_at='2012-10-10 12:12:12')
        self.assertIsNone(c.id)
        self.assertEqual(u.card, c)

    # def test_init_with_assoication_record(self):
    #     u = User.create(name='pengyi')
    #     c = Card(created_at='2012-10-10 12:12:12', user=u)
    #     self.assertIsNone(c.id)
    #     self.assertEqual(u.card, c)


if __name__ == '__main__':
    unittest.main()
