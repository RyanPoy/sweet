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
        self.assertTrue(r.owner_class is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target_class is Phone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('phone', r.owner_attr)

    def test_has_one_relation_init_without_owner_attr(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
    
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
                
        r = HasOne(target_class=CardPhone, owner_class=User, foreign_key='user_id')
        self.assertTrue(r.owner_class is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target_class is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phone', r.owner_attr)
 
    def test_has_one_relation_init_without_foreign_key(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
        
        r = HasOne(target_class=CardPhone, owner_class=User, owner_attr='card_phone')
        self.assertTrue(r.owner_class is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target_class is CardPhone)
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
              
        r = HasOne(target_class="sweet.tests.units.test_relation_has_one.Card", owner_class=User)
        self.assertTrue(r.owner_class is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target_class is Card)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card', r.owner_attr)
 
    @fudge.patch('sweet.record.Criteria')
    def test_user_has_one_phone_relation(self, Criteria):
        ActiveRecord.__dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_one(Phone)
  
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(User).returns_fake()\
                .expects('where').with_args(id=(10, )).returns_fake()\
                .expects('first').returns(User(id=10, created_at=datetime.now(), updated_at=datetime.now(), name='py'))
        u = User.find(10)
        self.assertEqual('py', u.name)
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('first').returns(Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10))
        p = u.phone
        self.assertEqual(1, p.id)
        self.assertTrue(isinstance(p.created_at, datetime))
        self.assertTrue(isinstance(p.updated_at, datetime))
        self.assertEqual(10, p.user_id)

    # @fudge.patch('sweet.record.Criteria')
    # def test_delete_user_should_delete_phone_because_user_has_one_phone_relation(self, Criteria):
    #     ActiveRecord.__dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
    #     class Phone(ActiveRecord):
    #         __columns__ = ['id', 'created_at', 'updated_at', 'user_id']

    #     class User(ActiveRecord):
    #         __columns__ = ['id', 'name', 'created_at', 'updated_at']
    #         has_one(Phone)
  
    #     u = User(id=10, created_at=datetime.now(), updated_at=datetime.now(), name='py')
    #     u._ActiveRecord__is_persisted = True
        
    #     Criteria.is_callable().returns_fake()\
    #             .expects('set_record_class').with_args(Phone).returns_fake()\
    #             .expects('where').with_args(user_id=10).returns_fake()\
    #             .expects('delete').returns(1)
        
    #     c = fudge.Fake('criteria')
    #     u._new_criteria = lambda: c
    #     c.expects('where').with_args(id=10).returns_fake()\
    #      .expects('delete').returns(True)   
    #     u.delete()


if __name__ == '__main__':
    unittest.main()
