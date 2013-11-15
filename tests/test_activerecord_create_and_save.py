#coding: utf8
from pyrails.tests import create_table, drop_table
from pyrails.record import ActiveRecord
from pyrails.associations import has_one
from pyrails.active_support import datetime2str
import unittest


class ActiveRecordCreateAndSaveTest(unittest.TestCase):

    def setUp(self):
        drop_table('users')
        create_table("""
create table if not exists users (
    id int auto_increment,
    username varchar(32) not null,
    age int default 0,
    password varchar(32),
    PRIMARY KEY (id)
);""")
        
    def tearDown(self):
        drop_table('users')

    def test_create(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertTrue(u.id is not None)

    def test_should_find_record_after_create(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        u = User.find(1)
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertEqual(1, u.id)

    def test_save(self):
        class User(ActiveRecord): pass
        u = User.create(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertTrue(u.id is not None)

    def test_should_find_record_after_save(self):
        class User(ActiveRecord): pass
        User.create(username="abc", password="123")
        u = User.find(1)
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)
        self.assertEqual(1, u.id)

    def test_has_one_create(self):
        try:
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
            class Card(ActiveRecord): pass
            class User(ActiveRecord): has_one(Card)

            u = User.create(name='pengyi')
            c = u.create_card(created_at='2012-10-10 12:12:12')
            self.assertIsNotNone(c.id)
            self.assertEqual(u.card, c)

            c2 = Card.find(c.id)
            self.assertEqual(datetime2str(c2.created_at), c.created_at)
        finally:
            drop_table('users')
            drop_table('cards')

if __name__ == '__main__':
    unittest.main()
