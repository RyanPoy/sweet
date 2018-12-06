#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.recordset import MySQLRecordset
from sweet.database.db import Record
from sweet.database.clauses import WhereClause, HavingClause


class MySQLRecordsetQueryTest(TestCase):

    def get_recordset(self, name="users"):
        class FakeDB(object): pass
        return MySQLRecordset(db=FakeDB(), tbname=name)

    def test_copy_deep(self):
        tb = self.get_recordset()
        self.assertEqual('SELECT `id`, `name` FROM `users`', tb.select('id').select('name').sql)
        self.assertEqual([], tb.bindings)

        tb = tb.select('*').where(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_basic_select(self):
        tb = self.get_recordset().select('*')
        self.assertEqual('SELECT * FROM `users`', tb.sql)

    def test_basic_distinct(self):
        tb = self.get_recordset().distinct().select('id')
        self.assertEqual('SELECT DISTINCT `id` FROM `users`', tb.sql)

    def test_default_select(self):
        self.assertEqual('SELECT * FROM `users`', self.get_recordset().sql)

    def test_multi_select(self):
        tb = self.get_recordset().select('id', 'name')
        self.assertEqual('SELECT `id`, `name` FROM `users`', tb.sql)

    def test_select_twice(self):
        tb = self.get_recordset().select('id').select('name')
        self.assertEqual('SELECT `id`, `name` FROM `users`', tb.sql)

    def test_where(self):
        tb = self.get_recordset().select('*').where(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_where_not(self):
        tb = self.get_recordset().select('*').where(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` <> %s AND `name` <> %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_where_in(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_where_not_in(self):
        tb = self.get_recordset().select('*').where(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) AND `name` <> %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_where_like(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], name__like='%yanpo%')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` LIKE %s', tb.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], tb.bindings)

    def test_where_not_like(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], name__not_like='%yanpo%')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` NOT LIKE %s', tb.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], tb.bindings)

    def test_where_in_empty(self):
        tb = self.get_recordset().select('*').where(id=[], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN () AND `name` = %s', tb.sql)
        self.assertEqual(['ryanpoy'], tb.bindings)

    def test_where_in_none(self):
        tb = self.get_recordset().select('*').where(id=[None, None], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([None, None, 'ryanpoy'], tb.bindings)

    def test_where_null(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_where_not_null(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3]).where(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_where_gt(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], age__gt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` > %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_gte(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], age__gte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` >= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_lt(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], age__lt=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` < %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_lte(self):
        tb = self.get_recordset().select('*').where(id=[1, 2, 3], age__lte=30)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` <= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_where_between(self):
        tb = self.get_recordset().select('*').where(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_where_not_between(self):
        tb = self.get_recordset().select('*').where(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_where_between_should_give_me_error(self):
        tb = self.get_recordset().select('*')
        with self.assertRaises(TypeError):
            tb.where(id__bt=[1, 2, 3])

    def test_where_group_parameters(self):
        tb = self.get_recordset().select('*').where(id__bt=[1, 2]).or_where(
            WhereClause('`', '%s').and_(name='jim').or_(name='lucy')
        ).where(id=10)
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s OR ( `name` = %s OR `name` = %s ) AND `id` = %s', tb.sql)
        self.assertEqual([1, 2, 'jim', 'lucy', 10], tb.bindings)

    def test_or(self):
        tb = self.get_recordset().select('*').or_where(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s OR `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_not(self):
        tb = self.get_recordset().select('*').or_where(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` <> %s OR `name` <> %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_between(self):
        tb = self.get_recordset().select('*').where(name='ryanpoy').or_where(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_not_between(self):
        tb = self.get_recordset().select('*').where(name='ryanpoy').or_where(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_in(self):
        tb = self.get_recordset().select('*').or_where(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_or_not_in(self):
        tb = self.get_recordset().select('*').or_where(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) OR `name` <> %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)
    
    def test_or_null(self):
        tb = self.get_recordset().select('*').or_where(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_not_null(self):
        tb = self.get_recordset().select('*').or_where(id=[1, 2, 3]).or_where(name__not=None)
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)
    
    def test_order_by(self):
        tb = self.get_recordset().select('*').order_by('email').order_by('age', 'desc')
        self.assertEqual('SELECT * FROM `users` ORDER BY `email`, `age` DESC', tb.sql)

    def test_group_bys(self):
        tb = self.get_recordset().select('*').group_by('id', 'email')
        self.assertEqual('SELECT * FROM `users` GROUP BY `id`, `email`', tb.sql )

    def test_where_group_parameters(self):
        tb = self.get_recordset().select('*').having(id=1, name='ryanpoy').or_having(
            WhereClause('`', '%s').and_(name='jim').or_(name='lucy')
        )
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s OR ( `name` = %s OR `name` = %s )', tb.sql)
        self.assertEqual([1, 'ryanpoy', 'jim', 'lucy'], tb.bindings)

    def test_having(self):
        tb = self.get_recordset().select('*').having(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_having_in(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_having_null(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_having_gt(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], age__gt=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` > %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_gte(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], age__gte=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` >= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_lt(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], age__lt=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` < %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_lte(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3], age__lte=30)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` <= %s', tb.sql)
        self.assertEqual([1, 2, 3, 30], tb.bindings)

    def test_having_between(self):
        tb = self.get_recordset().select('*').having(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_having_between_should_give_me_error(self):
        tb = self.get_recordset().select('*')
        with self.assertRaises(TypeError):
            tb.having(id__bt=[1, 2, 3])

    def test_having_not(self):
        tb = self.get_recordset().select('*').having(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` <> %s AND `name` <> %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_having_not_null(self):
        tb = self.get_recordset().select('*').having(id=[1, 2, 3]).having(name__not=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_having_not_in(self):
        tb = self.get_recordset().select('*').having(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) AND `name` <> %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_having_not_between(self):
        tb = self.get_recordset().select('*').having(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual([1, 2], tb.bindings)

    def test_or_having(self):
        tb = self.get_recordset().select('*').or_having(id=1, name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s OR `name` = %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_having_between(self):
        tb = self.get_recordset().select('*').having(name='ryanpoy').or_having(id__bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_having_in(self):
        tb = self.get_recordset().select('*').or_having(id=[1, 2, 3], name='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_or_having_null(self):
        tb = self.get_recordset().select('*').or_having(id=[1, 2, 3], name=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_having_not_between(self):
        tb = self.get_recordset().select('*').having(name='ryanpoy').or_having(id__not_bt=[1, 2])
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` NOT BETWEEN %s AND %s', tb.sql)
        self.assertEqual(['ryanpoy', 1, 2], tb.bindings)

    def test_or_having_not(self):
        tb = self.get_recordset().select('*').or_having(id__not=1, name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` <> %s OR `name` <> %s', tb.sql)
        self.assertEqual([1, 'ryanpoy'], tb.bindings)

    def test_or_having_not_null(self):
        tb = self.get_recordset().select('*').or_having(id=[1, 2, 3]).or_having(name__not=None)
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NOT NULL', tb.sql)
        self.assertEqual([1, 2, 3], tb.bindings)

    def test_or_having_not_in(self):
        tb = self.get_recordset().select('*').or_having(id__not=[1, 2, 3], name__not='ryanpoy')
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) OR `name` <> %s', tb.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_limits_and_offsets(self):
        tb = self.get_recordset().select('*').offset(5).limit(10)
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 5', tb.sql)

    def test_page(self):
        tb = self.get_recordset().select('*').page(2, 15)
        self.assertEqual('SELECT * FROM `users` LIMIT 15 OFFSET 15', tb.sql)

    def test_join(self):
        tb = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="ryanpoy")\
          .join('cars', "users.id=cars.user_id").where(cars__name='focus')
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s AND `cars`.`name` = %s', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy', 'focus'], tb.bindings)

    def test_join_without_select(self):
        tb = self.get_recordset().where(id=[1,2,3]).or_where(name="ryanpoy").join('cars', "users.id=cars.user_id").where(cars__name='focus')
        self.assertEqual('SELECT * FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s AND `cars`.`name` = %s', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy', 'focus'], tb.bindings)

    def test_join_with_function(self):
        def complex(join):
            join.on('users.id=cars.user_id').and_(cars__id=10).or_(users__id=100)
        tb = self.get_recordset().join('cars', func=complex)

        self.assertEqual('SELECT * FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `cars`.`id` = %s OR `users`.`id` = %s', tb.sql)
        self.assertEqual([10, 100], tb.bindings)

    def test_left_join(self):
        tb = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="ryanpoy").left_join('cars', "users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_right_join(self):
        tb = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="ryanpoy").right_join('cars', "users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` RIGHT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_cross_join(self):
        tb = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="ryanpoy").cross_join('cars', "users.id=cars.user_id")
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` CROSS JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_count(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count()
        self.assertEqual(2, cnt)

    def test_count_with_column(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count('id')
        self.assertEqual(2, cnt)

    def test_count_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count(distinct=True)
        self.assertEqual(2, cnt)

    def test_count_distinct_with_column(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count('id', True)
        self.assertEqual(2, cnt)

    def test_max(self):
        def _(sql, *params):
            self.assertEqual('SELECT MAX(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 1024})
        tb = self.get_recordset()
        tb.db.fetchone = _
        max_value = tb.where(name__not='Lily').max('id')
        self.assertEqual(1024, max_value)

    def test_max_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT MAX(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 1024})
        tb = self.get_recordset()
        tb.db.fetchone = _
        max_value = tb.where(name__not='Lily').max('id', True)
        self.assertEqual(1024, max_value)

    def test_min(self):
        def _(sql, *params):
            self.assertEqual('SELECT MIN(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 1})
        tb = self.get_recordset()
        tb.db.fetchone = _
        min_value = tb.where(name__not='Lily').min('id')
        self.assertEqual(1, min_value)

    def test_min_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT MIN(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 1})
        tb = self.get_recordset()
        tb.db.fetchone = _
        min_value = tb.where(name__not='Lily').min('id', True)
        self.assertEqual(1, min_value)

    def test_avg(self):
        def _(sql, *params):
            self.assertEqual('SELECT AVG(`age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 32})
        tb = self.get_recordset()
        tb.db.fetchone = _
        avg = tb.where(name__not='Lily').avg('age')
        self.assertEqual(32, avg)

    def test_avg_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT AVG(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 32})
        tb = self.get_recordset()
        tb.db.fetchone = _
        avg = tb.where(name__not='Lily').avg('age', True)
        self.assertEqual(32, avg)

    def test_sum(self):
        def _(sql, *params):
            self.assertEqual('SELECT SUM(`age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2048})
        tb = self.get_recordset()
        tb.db.fetchone = _
        sum_value = tb.where(name__not='Lily').sum('age')
        self.assertEqual(2048, sum_value)

    def test_sum_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT SUM(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return Record({'aggregate': 2048})
        tb = self.get_recordset()
        tb.db.fetchone = _
        sum_value = tb.where(name__not='Lily').sum('age', True)
        self.assertEqual(2048, sum_value)

    def test_exists_distinct(self):
        tb = self.get_recordset()
        tb.first = lambda: {'id': 1, 'name': 'Poy', 'age': 25}
        exists = tb.where(name__not='Lily').exists()
        self.assertEqual(True, exists)
    
    def test_read_lock(self):
        tb = self.get_recordset().select('users.id').select('cars.name')\
                 .where(id=[1,2,3]).or_where(name="ryanpoy")\
                 .left_join('cars', "users.id=cars.user_id").read_lock()
        self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s LOCK IN SHARE MODE', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_write_lock(self):
        tb = self.get_recordset().select('users.id').select('cars.name')\
                 .where(id=[1,2,3]).or_where(name="ryanpoy")\
                 .left_join('cars', "users.id=cars.user_id").write_lock()
        self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s FOR UPDATE', tb.sql)
        self.assertEqual([ 1, 2, 3, 'ryanpoy'], tb.bindings)

    def test_where_exists(self):
        users = self.get_recordset()
        mobiles = self.get_recordset("mobiles")

        users = users.where(age__lte=20).where_exists(
            mobiles.where(name='iphone')
        )
        self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
        self.assertEqual([20, 'iphone'], users.bindings)

    def test_multiple_where_exists(self):
        users = self.get_recordset().where_exists(
            self.get_recordset("mobiles").where(name='iphone'),
            self.get_recordset("mobiles").where(name='aphone')
        )
        self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
        self.assertEqual(['iphone', 'aphone'], users.bindings)

    def test_or_exists(self):
        users = self.get_recordset()
        mobiles = self.get_recordset("mobiles")

        users = users.where(age__lte=20).or_exists(
            mobiles.where(name='iphone')
        )
        self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
        self.assertEqual([20, 'iphone'], users.bindings)

    def test_multiple_or_exists(self):
        users = self.get_recordset().or_exists(
            self.get_recordset("mobiles").where(name='iphone'),
            self.get_recordset("mobiles").where(name='aphone')
        )
        self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
        self.assertEqual(['iphone', 'aphone'], users.bindings)

    def test_complex_where_or_exists(self):
        users = self.get_recordset().where(age__lte=20).where_exists(
                    self.get_recordset("mobiles").where(name='iphone'),
                ).or_exists(
                    self.get_recordset("mobiles").where(name='iphone'),
                    self.get_recordset("mobiles").where(name='aphone')
                ).where_exists(
                    self.get_recordset("mobiles").where(name='aphone')
                )
        sql = 'SELECT * FROM `users` WHERE `age` <= %s' \
              ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)'
        self.assertEqual(sql, users.sql)
        self.assertEqual([20, 'iphone', 'iphone', 'aphone', 'aphone'], users.bindings)

    def test_unions(self):
        users = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        )
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s UNION SELECT * FROM `users` WHERE `id` = %s',
            users.sql  
        )
        self.assertEqual([1, 2], users.bindings)

    def test_union_alls(self):
        users = self.get_recordset().where(id=1).union_all(
            self.get_recordset().where(id=2)
        )
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s UNION ALL SELECT * FROM `users` WHERE `id` = %s',
            users.sql  
        )
        self.assertEqual([1, 2], users.bindings)

    def test_multiple_unions(self):
        users = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).union(
            self.get_recordset().where(id=3)
        )
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s',
            users.sql
        )
        self.assertEqual([1, 2, 3], users.bindings)

    def test_multiple_union_alls(self):
        users = self.get_recordset().where(id=1).union_all(
            self.get_recordset().where(id=2)
        ).union_all(
            self.get_recordset().where(id=3)
        )
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION ALL SELECT * FROM `users` WHERE `id` = %s '
            'UNION ALL SELECT * FROM `users` WHERE `id` = %s',
            users.sql
        )
        self.assertEqual([1, 2, 3], users.bindings)

    def test_union_order_by(self):
        users = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).order_by('id', True)
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'ORDER BY `id` DESC',
            users.sql
        )
        self.assertEqual([1, 2], users.bindings)

    def test_union_limits_and_offsets(self):
        users = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).limit(10).offset(20)
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'LIMIT 10 OFFSET 20',
            users.sql
        )
        self.assertEqual([1, 2], users.bindings)

    def test_union_order_by_and_limits_and_offsets(self):
        users = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).limit(10).offset(20).order_by('id', False)
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'ORDER BY `id` LIMIT 10 OFFSET 20',
            users.sql
        )
        self.assertEqual([1, 2], users.bindings)


if __name__ == '__main__':
    import unittest
    unittest.main()

    