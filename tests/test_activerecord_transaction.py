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
from pyrails.activerecord import ActiveRecord
import unittest


class ActiveRecordTransationTest(unittest.TestCase):

    def setUp(self):
        self.user_table_name = 'users'
        drop_table(self.user_table_name)
        create_table("""
create table if not exists %s (
    id int auto_increment,
    name varchar(32) not null,
    age int default 0,
    password varchar(32),
    PRIMARY KEY (id)
);""" % self.user_table_name)
        
    def tearDown(self):
        drop_table(self.user_table_name)

    def test_should_find_a_record_if_insert_a_record_when_transaction_commit(self):
        class User(ActiveRecord): pass
        with User.transaction:
            User.create(name="pengyi", age=33, password="pwd")
        u = User.find(1)
        self.assertEqual('pengyi', u.name)

    def test_should_not_find_if_insert_a_record_when_transaction_rollback(self):
        class User(ActiveRecord): pass
        try:
            with User.transaction:
                User.create(name="pengyi", age=33, password="pwd")
                raise Exception
        except:
            pass
        self.assertEqual(0, User.count())

    def test_should_not_find_if_insert_record_and_find_it_and_rollback_transaction(self):
        class User(ActiveRecord): pass
        try:
            with User.transaction:
                User.create(name="pengyi", age=33, password="pwd")
                u = User.find(1)
                self.assertEqual(1, u.id)
                raise Exception
        except AssertionError:
            raise
        except:
            pass
        self.assertEqual(0, User.count())


if __name__ == '__main__':
    unittest.main()
