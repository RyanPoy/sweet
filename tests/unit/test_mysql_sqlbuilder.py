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

    def test_where_gt(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], age__gt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` > %s', sb.sql)
        self.assertEqual([1, 2, 3, 30], sb.bindings)

    def test_where_gte(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], age__gte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` >= %s', sb.sql)
        self.assertEqual([1, 2, 3, 30], sb.bindings)

    def test_where_lt(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], age__lt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` < %s', sb.sql)
        self.assertEqual([1, 2, 3, 30], sb.bindings)

    def test_where_lte(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3], age__lte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` <= %s', sb.sql)
        self.assertEqual([1, 2, 3, 30], sb.bindings)

    def test_where_between(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s', sb.sql)
        self.assertEqual([1, 2], sb.bindings)

    def test_where_between_should_give_me_error(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id__bt=[1, 2, 3])
        with self.assertRaises(TypeError):
            sb.sql

    def test_where_not(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s AND `name` != %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_where_not_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id=[1, 2, 3]).where(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_where_not_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) AND `name` != %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)

    def test_where_not_between(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT BETWEEN %s AND %s', sb.sql)
        self.assertEqual([1, 2], sb.bindings)

    def test_or(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s OR `name` = %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_or_between(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(name='ryanpoy').or_(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` BETWEEN %s AND %s', sb.sql)
        self.assertEqual(['ryanpoy', 1, 2], sb.bindings)

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

    def test_or_not_between(self):
        sb = self.get_builder()
        sb.select('*').from_('users').where(name='ryanpoy').or_(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` NOT BETWEEN %s AND %s', sb.sql)
        self.assertEqual(['ryanpoy', 1, 2], sb.bindings)

    def test_or_not(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s OR `name` != %s', sb.sql)
        self.assertEqual([1, 'ryanpoy'], sb.bindings)

    def test_or_not_null(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id=[1, 2, 3]).or_(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NOT NULL', sb.sql)
        self.assertEqual([1, 2, 3], sb.bindings)

    def test_or_not_in(self):
        sb = self.get_builder()
        sb.select('*').from_('users').or_(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) OR `name` != %s', sb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], sb.bindings)
        
    def test_order_by(self):
        sb = self.get_builder()
        sb.select('*').from_('users').order_by('email').order_by('age', 'desc')
        self.assertEqual('SELECT * FROM `users` ORDER BY `email`, `age` DESC', sb.sql)

    def test_group_bys(self):
        sb = self.get_builder()
        sb.select('*').from_('users').group_by('id', 'email')
        self.assertEqual('SELECT * FROM `users` GROUP BY `id`, `email`', sb.sql )

if __name__ == '__main__':
    import unittest
    unitest.testmain()

    