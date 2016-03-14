# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.relation import has_many, HasMany
from sweet.utils import ColumnNotInColumns
from datetime import datetime
import unittest
import fudge


class Card(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at', 'user_id']


class RelationHasManyTestCase(unittest.TestCase):

    def test_has_many_relation_init(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
    
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']

        r = HasMany(target_class=CardPhone, owner_class=User, foreign_key='user_id', owner_attr='card_phones')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phones', r.owner_attr)

    def test_has_many_relation_init_without_owner_attr(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
     
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
                 
        r = HasMany(target_class=CardPhone, owner_class=User, foreign_key='user_id')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phones', r.owner_attr)
  
    def test_has_many_relation_init_without_foreign_key(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
    
        class CardPhone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
         
        r = HasMany(target_class=CardPhone, owner_class=User, owner_attr='card_phones')
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is CardPhone)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('card_phones', r.owner_attr)
 
    def test_has_many_relation_init_should_raise_exception_if_relation_foreign_key_not_in_columns(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
   
        r = HasMany(target_class=Phone, owner_class=User)
        self.assertRaises(ColumnNotInColumns, lambda: r.foreign_key)
  
    def test_has_many_relation_init_with_target_classpath(self):
        class User(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at']
               
        r = HasMany(target_class="sweet.tests.units.test_relation_has_many.Card", owner_class=User)
        self.assertTrue(r.owner is User)
        self.assertEqual('user_id', r.foreign_key)
        self.assertTrue(r.target is Card)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('cards', r.owner_attr)

    @fudge.patch('sweet.record.Criteria')
    def test_user_has_many_phone_relation(self, Criteria):
        ActiveRecord.__dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)

        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(User).returns_fake()\
                .expects('where').with_args(id=(10, )).returns_fake()\
                .expects('first').returns(User(id=10, created_at=datetime.now(), updated_at=datetime.now(), name='py'))
        u = User.find(10)
        self.assertEqual('py', u.name)
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('all').returns([
                    Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                    Phone(id=2, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                    Phone(id=3, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                ])
        ps = u.phones
        self.assertEqual(3, len(ps))
        p = ps[0]
        self.assertEqual(1, p.id)
        self.assertTrue(isinstance(p.created_at, datetime))
        self.assertTrue(isinstance(p.updated_at, datetime))
        self.assertEqual(10, p.user_id)


if __name__ == '__main__':
    unittest.main()
