#coding: utf8
from pyrails.associations import belongs_to, Association, has_one
from pyrails.tests import create_table, drop_table
from pyrails.support import datetime2str
from pyrails.record import ActiveRecord
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
        association = Child.association_dict['father']
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
        association = Child.association_dict['father']
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
        association = Child.association_dict['dad']
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

    def test_belongs_to_query(self):
        class Father(ActiveRecord): pass
        class Child(ActiveRecord): belongs_to(Father)
    
        fid = Father.create(name='Bob').id
        c = Child.create(created_at='2011-10-10 12:12:12', father_id=fid)
        f = c.father
        self.assertEqual(fid, f.id)
        self.assertEqual('Bob', f.name)

    def test_has_one_join_query(self):
        f = Father.create(name='pengyi')
        f.create_child(created_at='2012-10-10 12:12:12')

        c = Child.joins('father').where('fathers.name = ?', 'poy').first
        self.assertTrue(c is None)

        c = Child.joins('father').where('fathers.name = ?', 'pengyi').first
        self.assertEqual('2012-10-10 12:12:12', datetime2str(c.created_at))


if __name__ == '__main__':
    unittest.main()
