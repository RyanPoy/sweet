# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.utils import ColumnNotInColumns
from sweet.relation import belongs_to, BelongsTo
from datetime import datetime
import unittest
import fudge


class Person(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at']


class RelationBelongsToTestCase(unittest.TestCase):

    def test_belongs_to_relation_init(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
               
        r = BelongsTo(target_class=User, owner_class=Phone, foreign_key='user_id', owner_attr='user')
        self.assertTrue(r.owner is Phone)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is User)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('user', r.owner_attr)
    
    def test_belongs_to_relation_init_without_owner_attr(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
               
        r = BelongsTo(target_class=User, owner_class=Phone, foreign_key='user_id')
        self.assertTrue(r.owner is Phone)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is User)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('user', r.owner_attr)

    def test_belongs_to_relation_init_without_foreign_key(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
  
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
              
        r = BelongsTo(target_class=User, owner_class=Phone, owner_attr='user')
        self.assertTrue(r.owner is Phone)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is User)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('user', r.owner_attr)
         
    def test_belongs_to_relation_init_should_raise_exception_if_relation_foreign_key_not_in_columns(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
 
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
 
        r = BelongsTo(target_class=User, owner_class=Phone)
        self.assertRaises(ColumnNotInColumns, lambda: r.foreign_key)

    def test_belongs_to_relation_init_with_target_classpath(self):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'person_id']
             
        r = BelongsTo(target_class="sweet.tests.units.test_relation_belongs_to.Person", owner_class=Phone, owner_attr='user')
        self.assertTrue(r.owner is Phone)
        self.assertEqual('person_id', r.foreign_key)
        self.assertTrue(r.target is Person)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('user', r.owner_attr)

    @fudge.patch('sweet.record.Criteria')
    def test_phone_belongs_to_user_relation(self, Criteria):
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
  
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            belongs_to(User)
 
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=(1, )).returns_fake()\
                .expects('first').returns(Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10))
        p = Phone.find(1)
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=10).returns_fake()\
                .expects('first').returns(User(id=10, name='py', created_at=datetime.now(), updated_at=datetime.now()))
        u = p.user
        self.assertEqual('py', u.name)
        self.assertEqual(10, u.id)
        self.assertTrue(isinstance(u.created_at, datetime))
        self.assertTrue(isinstance(u.updated_at, datetime))

        
if __name__ == '__main__':
    unittest.main()
