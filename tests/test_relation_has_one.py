# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.relation import has_one, HasOne
from sweet.utils import ColumnNotInColumns
from datetime import datetime
import unittest
import fudge


class Card(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at', 'user_id']


class RelationHasOneTestCase(unittest.TestCase):

    def test_has_one_relation_init(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
    
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']

        r = HasOne(target_class=Phone, owner_class=User, foreign_key='user_id', owner_attr='phone')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is Phone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('phone', r.owner_attr)

    def test_has_one_relation_init_without_owner_attr(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
    
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
                
        r = HasOne(target_class=CardPhone, owner_class=User, foreign_key='user_id')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phone', r.owner_attr)
 
    def test_has_one_relation_init_without_foreign_key(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
        
        r = HasOne(target_class=CardPhone, owner_class=User, owner_attr='card_phone')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phone', r.owner_attr)

    def test_has_one_relation_init_should_raise_exception_if_relation_foreign_key_not_in_columns(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
  
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
  
        r = HasOne(target_class=Phone, owner_class=User)
        self.assertRaises(ColumnNotInColumns, lambda: r.foreign_key)
 
    def test_has_one_relation_init_with_target_classpath(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
              
        r = HasOne(target_class="sweet.tests.test_relation_has_one.Card", owner_class=User)
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is Card)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card', r.owner_attr)
 
    @fudge.patch('sweet.record.Criteria')
    def test_user_has_one_phone_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_one(Phone)
  
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=(10, )).returns_fake()\
                .expects('first').returns(User(id=10, created_at=datetime.now(), updated_at=datetime.now(), name='py'))
        u = User.find(10)
        self.assertEqual('py', u.name)
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('first').returns(Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10))
        p = u.phone
        self.assertEqual(1, p.id)
        self.assertTrue(isinstance(p.created_at, datetime))
        self.assertTrue(isinstance(p.updated_at, datetime))
        self.assertEqual(10, p.user_id)
        

if __name__ == '__main__':
    unittest.main()
