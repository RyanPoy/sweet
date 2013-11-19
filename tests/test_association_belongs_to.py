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
from pyrails.active_record import ActiveRecord, belongs_to, has_one
from pyrails.tests import create_table, drop_table
import unittest


class Father(ActiveRecord): 
    has_one('pyrails.tests.test_association_belongs_to.Child')

class Child(ActiveRecord):
    belongs_to(Father)


class BelongsToTest(unittest.TestCase):
    
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
        
    # belongs to association define
    def test_belongs_to(self):
        class Father(ActiveRecord): pass
        class Child(ActiveRecord):
            belongs_to(Father)
        
        self.assertEqual(1, len(Child.association_dict))
        association = Child.association_of('father')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Father, association.target)
        self.assertEqual('father', association.attr_name)
        self.assertEqual('father_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_belongs_to_with_classpath_str(self):
        from pyrails.tests.test_helper import Father
        class Child(ActiveRecord):
            belongs_to('pyrails.tests.test_helper.Father')
        
        self.assertEqual(1, len(Child.association_dict))
        association = Child.association_of('father')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Father, association.target)
        self.assertEqual('father', association.attr_name)
        self.assertEqual('father_id', association.foreign_key)
        self.assertFalse(association.dependent)
        
    def test_belongs_to_with_customer_init_args(self):
        class Father(ActiveRecord): pass
        class Child(ActiveRecord):
            belongs_to(Father, attr_name="dad", foreign_key="dad_id", dependent=True)
        self.assertEqual(1, len(Child.association_dict))
        association = Child.association_of('dad')
        self.assertTrue(association.is_belongs_to())
        self.assertEqual(Father, association.target)
        self.assertEqual('dad', association.attr_name)
        self.assertEqual('dad_id', association.foreign_key)
        self.assertTrue(association.dependent)
    
    def test_belongs_to_create(self):
        class Father(ActiveRecord): pass
        class Child(ActiveRecord): belongs_to(Father)
    
        fid = Father.create(name='Bob').id
        cid = Child.create(created_at='2011-10-10 12:12:12', father_id=fid).id
        c = Child.find(cid)
        self.assertEqual(cid, c.id)
        self.assertEqual(fid, c.father_id)
        self.assertEqual('2011-10-10 12:12:12', c.created_at.strftime('%Y-%m-%d %H:%M:%S'))


if __name__ == '__main__':
    unittest.main()
