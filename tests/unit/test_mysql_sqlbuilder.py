#coding: utf8
from sweet.tests.unit import TestCase
from sweet.query import MysqlSQLBuilder


class MysqlSQLBuilderTest(TestCase):

    def get_builder(self):
        return MysqlSQLBuilder()

    def test_basic_select(self):
        sb = self.get_builder()
        sb.select('*').from_('users')
        self.assertEqual('SELECT * FROM `users`', sb.sql)

    def test_default_select(self):
        sb = self.get_builder()
        sb.from_('users')
        self.assertEqual('SELECT * FROM `users`', sb.sql)

    def test_multi_select(self):
        sb = self.get_builder()
        sb.select('id', 'name').from_('users')
        self.assertEqual('SELECT `id`, `name` FROM `users`', sb.sql)

    def test_select_twice(self):
        sb = self.get_builder()
        sb.select('id').from_('users').select('name')
        self.assertEqual('SELECT `id`, `name` FROM `users`', sb.sql)

    def test_where(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_where_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)

    def test_where_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_where_not(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where_not(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s AND `name` != %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_where_not_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3]).where_not(name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_where_not_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where_not(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) AND `name` != %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)

    def test_or_(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s OR `name` = %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_or_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)

    def test_or_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_or_not(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_not(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s OR `name` != %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_or_not_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=[1, 2, 3]).or_not(name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NOT NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_or_not_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_not(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) OR `name` != %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)

if __name__ == '__main__':
    import unittest
    unitest.testmain()

    