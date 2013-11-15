#coding: utf8
from pyrails.active_record import belongs_to, Association, has_one
from pyrails.tests import create_table, drop_table
from pyrails.active_record import ActiveRecord
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
        self.assertEqual(Association.Type.belongs_to, association._type)
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
        self.assertEqual(Association.Type.belongs_to, association._type)
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
        self.assertEqual(Association.Type.belongs_to, association._type)
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
