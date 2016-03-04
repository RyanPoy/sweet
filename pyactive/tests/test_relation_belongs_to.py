# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from ..relation import belongs_to, BelongsTo
import unittest
from pyactive.utils import ColumnNotInColumns

class Person(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at']


class RelationBelongsToTestCase(unittest.TestCase):

    def test_belongs_to_relation_init(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
               
        r = BelongsTo(target_class=User, owner_class=Phone, foreign_key='user_id')
        self.assertTrue(r.owner is Phone)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is User)
        self.assertEqual('id', r.target_pk_column)
     
    def test_belongs_to_relation_init_without_foreign_key(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
  
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
              
        r = BelongsTo(target_class=User, owner_class=Phone)
        self.assertTrue(r.owner is Phone)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is User)
        self.assertEqual('id', r.target_pk_column)
         
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
             
        r = BelongsTo(target_class="pyactive.tests.test_relation_belongs_to.Person", owner_class=Phone)
        self.assertTrue(r.owner is Phone)
        self.assertEqual('person_id', r.foreign_key)
        self.assertTrue(r.target is Person)
        self.assertEqual('id', r.target_pk_column)
        
    def test_phone_belongs_to_user_relation(self):
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
 
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            belongs_to(User)
 
        rs = User.__relations__
        self.assertEqual(1, len(rs))
        self.assertTrue(isinstance(rs[0], BelongsTo))
        self.assertEqual(Phone, rs[0].owner)
        self.assertEqual(User, rs[0].target)
        self.assertEqual('user_id', rs[0].foreign_key)
        self.assertEqual('id', rs[0].target_pk_column)

