#coding: utf8
from pyrails.record import ActiveRecord
from pyrails.associations import has_one
from pyrails.tests import drop_table, create_table
import unittest


class ActiveRecordInitTest(unittest.TestCase):

    def test_init(self):
        class User(ActiveRecord): pass
        u = User(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)

    def test_has_one_build(self):
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
            c = u.build_card(created_at='2012-10-10 12:12:12')
            self.assertIsNone(c.id)
            self.assertEqual(u.card, c)
        finally:
            drop_table('users')
            drop_table('cards')


if __name__ == '__main__':
    unittest.main()
