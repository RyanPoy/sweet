#coding: utf8
from pyrails.associations import Association, has_one
from pyrails.record import ActiveRecord
from pyrails.tests import drop_table, create_table
import unittest


class HasOneTest(unittest.TestCase):

    def setUp(self):
        drop_table('users')
        drop_table('cards')

        create_table("""
create table if not exists users (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")
        create_table("""
create table if not exists cards (
    id int auto_increment,
    created_at datetime,
    user_id int,
    PRIMARY KEY (id)
);
""")

    def tearDown(self):
        drop_table('users')
        drop_table('cards')

    # # has one association
    # def test_has_one(self):
    #     class Card(ActiveRecord): pass
    #     class User(ActiveRecord): has_one(Card)

    #     self.assertEqual(1, len(User.association_dict))
    #     association = User.association_dict['card']
    #     self.assertEqual(Association.Type.has_one, association._type)
    #     self.assertEqual(Card, association.target)
    #     self.assertEqual('card', association.attr_name)
    #     self.assertEqual('user_id', association.foreign_key)
    #     self.assertFalse(association.dependent)

    # def test_has_one_with_customer_init_args(self):
    #     class Card(ActiveRecord): pass
    #     class User(ActiveRecord): has_one(Card, attr_name="mycard", foreign_key='uid', dependent=True)

    #     self.assertEqual(1, len(User.association_dict))
    #     association = User.association_dict['mycard']
    #     self.assertEqual(Association.Type.has_one, association._type)
    #     self.assertEqual(Card, association.target)
    #     self.assertEqual('mycard', association.attr_name)
    #     self.assertEqual('uid', association.foreign_key)
    #     self.assertTrue(association.dependent)
    
    # def test_find_a_record_after_has_one_create(self):
    #     class Card(ActiveRecord): pass
    #     class User(ActiveRecord): has_one(Card)

    #     user = User.create(name='pengyi')
    #     card = user.create_card(created_at='2012-10-10 12:12:12')
    #     self.assertEqual(user.id, card.user_id)
    #     self.assertEqual(1, card.id)
    #     card = Card.find(1)
    #     self.assertEqual(1, card.id)
    #     self.assertEqual('2012-10-10 12:12:12', card.created_at.strftime('%Y-%m-%d %H:%M:%S'))

    # def test_find_in_user_a_record_after_has_one_create(self):
    #     class Card(ActiveRecord): pass
    #     class User(ActiveRecord): has_one(Card)
        
    #     user = User.create(name='pengyi')
    #     card = user.create_card(created_at='2012-10-10 12:12:12')
    #     self.assertEquals(card, user.card)

    # def test_has_one_create_with_customer_attr_name(self):
    #     class Card(ActiveRecord): pass
    #     class User(ActiveRecord): has_one(Card)
        
    #     user = User.create(name='pengyi')
    #     card1 = Card.create(user_id=user.id, created_at='2012-10-10 12:12:12')
    #     card2 = user.card
    #     self.assertEqual(card1.id, card2.id)
        
    def test_has_one_join_query(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_one(Card)
        c = User.create(name='pengyi').create_card(created_at='2012-10-10 12:12:12')
        u = User.joins("card").where(name='pengyi').first



if __name__ == '__main__':
    unittest.main()
