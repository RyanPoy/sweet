#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.table import MySQLTable


class MySQLTableTest(TestCase):

    def get_table(self):
        return MySQLTable()

    def test_basic_select(self):
        tb = self.get_table()
        tb.select('*').from_('users')
        self.assertEqual('SELECT * FROM `users`', tb.sql)

    def test_default_select(self):
        tb = self.get_table()
        tb.from_('users')
        self.assertEqual('SELECT * FROM `users`', tb.sql)

    def test_multi_select(self):
        tb = self.get_table()
        tb.select('id', 'name').from_('users')
        self.assertEqual('SELECT `id`, `name` FROM `users`', tb.sql)

    def test_select_twice(self):
        tb = self.get_table()
        tb.select('id').from_('users').select('name')
        self.assertEqual('SELECT `id`, `name` FROM `users`', tb.sql)

    def test_where(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_where_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_where_in_empty(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN () AND `name` = %s', tb.sql)
        self.assertEqual(['ryanpoy'], tb.bindings)

    def test_where_in_none(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[None, None], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([None, None, 'ryanpoy'], tb.bindings)

    def test_where_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_where_gt(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], age__gt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` > %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_gte(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], age__gte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` >= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_lt(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], age__lt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` < %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_lte(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3], age__lte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` <= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_where_between_should_give_me_error(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id__bt=[1, 2, 3])
        with self.assertRaises(TypeError):
            tb.sql

    def test_where_not(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s AND `name` != %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_where_not_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id=[1, 2, 3]).where(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_where_not_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) AND `name` != %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_where_not_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_or(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s OR `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(name='ryanpoy').or_(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_or_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_not_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').where(name='ryanpoy').or_(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_not(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` != %s OR `name` != %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_not_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id=[1, 2, 3]).or_(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_not_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) OR `name` != %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)
        
    def test_order_by(self):
        tb = self.get_table()
        tb.select('*').from_('users').order_by('email').order_by('age', 'desc')
        self.assertEqual('SELECT * FROM `users` ORDER BY `email`, `age` DESC', tb.sql)

    def test_group_bys(self):
        tb = self.get_table()
        tb.select('*').from_('users').group_by('id', 'email')
        self.assertEqual('SELECT * FROM `users` GROUP BY `id`, `email`', tb.sql )

    def test_having(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_having_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_having_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_having_gt(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], age__gt=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` > %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_gte(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], age__gte=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` >= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_lt(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], age__lt=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` < %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_lte(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3], age__lte=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` <= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_having_between_should_give_me_error(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id__bt=[1, 2, 3])
        with self.assertRaises(TypeError):
            tb.sql

    def test_having_not(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` != %s AND `name` != %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_having_not_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id=[1, 2, 3]).having(name__not=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_having_not_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) AND `name` != %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_having_not_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_or_having(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s OR `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_having_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(name='ryanpoy').or_having(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_having_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_or_having_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_having_not_between(self):
        tb = self.get_table()
        tb.select('*').from_('users').having(name='ryanpoy').or_having(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_having_not(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` != %s OR `name` != %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_having_not_null(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id=[1, 2, 3]).or_having(name__not=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_having_not_in(self):
        tb = self.get_table()
        tb.select('*').from_('users').or_having(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) OR `name` != %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_limits_and_offsets(self):
        tb = self.get_table()
        tb.select('*').from_('users').offset(5).limit(10)
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 5', tb.sql)

    def test_page(self):
        tb = self.get_table()
        tb.select('*').from_('users').page(2, 15)
        self.assertEqual('SELECT * FROM `users` LIMIT 15 OFFSET 15', tb.sql)

    def test_join(self):
        tb = self.get_table()
        tb.select('users.id').select('users.name').select('cars.name').from_('users').where(id=[1,2,3]).or_(name="ryanpoy").join('cars', on="users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_left_join(self):
        tb = self.get_table()
        tb.select('users.id').select('users.name').select('cars.name').from_('users').where(id=[1,2,3]).or_(name="ryanpoy").left_join('cars', on="users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_right_join(self):
        tb = self.get_table()
        tb.select('users.id').select('users.name').select('cars.name').from_('users').where(id=[1,2,3]).or_(name="ryanpoy").right_join('cars', on="users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` RIGHT JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)


if __name__ == '__main__':
    import unittest
    unitest.testmain()

    