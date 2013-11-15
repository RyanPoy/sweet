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
from pyrails.active_record import belongs_to, has_one
from pyrails.tests import create_table, drop_table
from pyrails.active_support import datetime2str
from pyrails.active_record import ActiveRecord
import unittest


class Father(ActiveRecord): 
    has_one('pyrails.tests.test_association_belongs_to.Child')

class Child(ActiveRecord):
    belongs_to(Father)


class ActiveRecordJoinsTest(unittest.TestCase):
    
    def setUp(self):
        drop_table('fathers')
        drop_table('children')
        
        create_table("""
create table if not exists fathers (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")
        create_table("""
create table if not exists children (
    id int auto_increment,
    created_at datetime,
    father_id int,
    PRIMARY KEY (id)
);
""")

    def tearDown(self):
        drop_table('fathers')
        drop_table('children')
        
    def test_belongs_to_query(self):
        fid = Father.create(name='Bob').id
        c = Child.create(created_at='2011-10-10 12:12:12', father_id=fid)
        f = c.father
        self.assertEqual(fid, f.id)
        self.assertEqual('Bob', f.name)

    def test_belongs_to_join(self):
        f = Father.create(name='pengyi')
        f.create_child(created_at='2012-10-10 12:12:12')

        c = Child.joins('father').where('fathers.name = ?', 'poy').first
        self.assertTrue(c is None)

        c = Child.joins('father').where('fathers.name = ?', 'pengyi').first
        self.assertEqual('2012-10-10 12:12:12', datetime2str(c.created_at))

    # def test_cache_of_belongs_to_join(self):
    #     f = Father.create(name='pengyi')
    #     f.create_child(created_at='2012-10-10 12:12:12')

    #     c = Child.first
    #     self.assertFalse('father' in c.__dict__)

    #     c = Child.joins('father').where('fathers.name = ?', 'pengyi').first
    #     self.assertTrue('father' in c.__dict__)

    def test_has_one_join_query(self):
        f = Father.create(name='pengyi')
        f.create_child(created_at='2012-10-10 12:12:12')

        f1 = Father.joins('child').where('children.created_at = ?', '2011-11-11 11:11:11').first
        self.assertIsNone(f1)
        
        f1 = Father.joins('child').where('children.created_at = ?', '2012-10-10 12:12:12').first
        self.assertEqual(f.id, f1.id)


if __name__ == '__main__':
    unittest.main()
