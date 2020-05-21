#coding: utf8
from sweet._tests import TestCase
from sweet.db.recordset import MySQLRecordset
from sweet.db.clauses import WhereClause, HavingClause
from sweet.utils import mydict


class TestRecordsetQueryForMySQL(TestCase):

    def get_recordset(self, name="users"):
        class FakeDB(object): pass
        return MySQLRecordset(db=FakeDB(), tbname=name)

    def test_copy_deep(self):
        tb = self.get_recordset()
        sql, params = tb.select('id').select('name')._query_sql()
        self.assertEqual('SELECT `id`, `name` FROM `users`', sql)
        self.assertEqual([], params)

        tb = tb.select('*').where(id=1, name='Ryan')
        sql, params = tb._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_basic_select(self):
        sql, params = self.get_recordset().select('*')._query_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)

    def test_basic_distinct(self):
        sql, params = self.get_recordset().distinct().select('id')._query_sql()
        self.assertEqual('SELECT DISTINCT `id` FROM `users`', sql)
        self.assertEqual([], params)

    def test_default_select(self):
        sql, params = self.get_recordset()._query_sql()
        self.assertEqual('SELECT * FROM `users`', sql)
        self.assertEqual([], params)

    def test_multi_select(self):
        sql, params = self.get_recordset().select('id', 'name')._query_sql()
        self.assertEqual('SELECT `id`, `name` FROM `users`', sql)
        self.assertEqual([], params)

    def test_select_twice(self):
        sql, params = self.get_recordset().select('id').select('name')._query_sql()
        self.assertEqual('SELECT `id`, `name` FROM `users`', sql)
        self.assertEqual([], params)

    def test_where_2(self):
        sql, params = self.get_recordset().select('*').where(users__id=1, users__name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `users`.`id` = %s AND `users`.`name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_where(self):
        sql, params = self.get_recordset().select('*').where(id=1, name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s AND `name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_where_not(self):
        sql, params = self.get_recordset().select('*').where(id__not=1, name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` <> %s AND `name` <> %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_where_in(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` = %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_where_not_in(self):
        sql, params = self.get_recordset().select('*').where(id__not=[1, 2, 3], name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) AND `name` <> %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_where_like(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], name__like='%yanpo%')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` LIKE %s', sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], params)

    def test_where_not_like(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], name__not_like='%yanpo%')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` NOT LIKE %s', sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], params)

    def test_where_in_empty(self):
        sql, params = self.get_recordset().select('*').where(id=[], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN () AND `name` = %s', sql)
        self.assertEqual(['Ryan'], params)

    def test_where_in_none(self):
        sql, params = self.get_recordset().select('*').where(id=[None, None], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s) AND `name` = %s', sql)
        self.assertEqual([None, None, 'Ryan'], params)

    def test_where_null(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], name=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_where_not_null(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3]).where(name__not=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_where_gt(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], age__gt=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` > %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_where_gte(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], age__gte=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` >= %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_where_lt(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], age__lt=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` < %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_where_lte(self):
        sql, params = self.get_recordset().select('*').where(id=[1, 2, 3], age__lte=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) AND `age` <= %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_where_between(self):
        sql, params = self.get_recordset().select('*').where(id__bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s', sql)
        self.assertEqual([1, 2], params)

    def test_where_not_between(self):
        sql, params = self.get_recordset().select('*').where(id__not_bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT BETWEEN %s AND %s', sql)
        self.assertEqual([1, 2], params)

    def test_where_between_should_give_me_error(self):
        tb = self.get_recordset().select('*')
        with self.assertRaises(TypeError):
            tb.where(id__bt=[1, 2, 3])

    def test_where_group_parameters(self):
        sql, params = self.get_recordset().select('*').where(id__bt=[1, 2]).or_where(
            WhereClause('`', '%s').and_(name='jim').or_(name='lucy')
        ).where(id=10)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` BETWEEN %s AND %s OR ( `name` = %s OR `name` = %s ) AND `id` = %s', sql)
        self.assertEqual([1, 2, 'jim', 'lucy', 10], params)

    def test_or(self):
        sql, params = self.get_recordset().select('*').or_where(id=1, name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s OR `name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_or_not(self):
        sql, params = self.get_recordset().select('*').or_where(id__not=1, name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` <> %s OR `name` <> %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_or_between(self):
        sql, params = self.get_recordset().select('*').where(name='Ryan').or_where(id__bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` BETWEEN %s AND %s', sql)
        self.assertEqual(['Ryan', 1, 2], params)

    def test_or_not_between(self):
        sql, params = self.get_recordset().select('*').where(name='Ryan').or_where(id__not_bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `name` = %s OR `id` NOT BETWEEN %s AND %s', sql)
        self.assertEqual(['Ryan', 1, 2], params)

    def test_or_in(self):
        sql, params = self.get_recordset().select('*').or_where(id=[1, 2, 3], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_or_not_in(self):
        sql, params = self.get_recordset().select('*').or_where(id__not=[1, 2, 3], name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` NOT IN (%s, %s, %s) OR `name` <> %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)
    
    def test_or_null(self):
        sql, params = self.get_recordset().select('*').or_where(id=[1, 2, 3], name=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_or_not_null(self):
        sql, params = self.get_recordset().select('*').or_where(id=[1, 2, 3]).or_where(name__not=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` IN (%s, %s, %s) OR `name` IS NOT NULL', sql)
        self.assertEqual([1, 2, 3], params)
    
    def test_order_by(self):
        sql, params = self.get_recordset().select('*').order_by('email').order_by('age', 'desc')._query_sql()
        self.assertEqual('SELECT * FROM `users` ORDER BY `email`, `age` DESC', sql)
        self.assertEqual([], params)

    def test_group_bys(self):
        sql, params = self.get_recordset().select('*').group_by('id', 'email')._query_sql()
        self.assertEqual('SELECT * FROM `users` GROUP BY `id`, `email`', sql)
        self.assertEqual([], params)

    def test_where_group_parameters(self):
        sql, params = self.get_recordset().select('*').having(id=1, name='Ryan').or_having(
            WhereClause('`', '%s').and_(name='jim').or_(name='lucy')
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s OR ( `name` = %s OR `name` = %s )', sql)
        self.assertEqual([1, 'Ryan', 'jim', 'lucy'], params)

    def test_having(self):
        sql, params = self.get_recordset().select('*').having(id=1, name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_having_in(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` = %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_having_null(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], name=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_having_gt(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], age__gt=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` > %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_having_gte(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], age__gte=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` >= %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_having_lt(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], age__lt=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` < %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_having_lte(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3], age__lte=30)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` <= %s', sql)
        self.assertEqual([1, 2, 3, 30], params)

    def test_having_between(self):
        sql, params = self.get_recordset().select('*').having(id__bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` BETWEEN %s AND %s', sql)
        self.assertEqual([1, 2], params)

    def test_having_between_should_give_me_error(self):
        tb = self.get_recordset().select('*')
        with self.assertRaises(TypeError):
            tb.having(id__bt=[1, 2, 3])

    def test_having_not(self):
        sql, params = self.get_recordset().select('*').having(id__not=1, name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` <> %s AND `name` <> %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_having_not_null(self):
        sql, params = self.get_recordset().select('*').having(id=[1, 2, 3]).having(name__not=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NOT NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_having_not_in(self):
        sql, params = self.get_recordset().select('*').having(id__not=[1, 2, 3], name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) AND `name` <> %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_having_not_between(self):
        sql, params = self.get_recordset().select('*').having(id__not_bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT BETWEEN %s AND %s', sql)
        self.assertEqual([1, 2], params)

    def test_or_having(self):
        sql, params = self.get_recordset().select('*').or_having(id=1, name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` = %s OR `name` = %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_or_having_between(self):
        sql, params = self.get_recordset().select('*').having(name='Ryan').or_having(id__bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` BETWEEN %s AND %s', sql)
        self.assertEqual(['Ryan', 1, 2], params)

    def test_or_having_in(self):
        sql, params = self.get_recordset().select('*').or_having(id=[1, 2, 3], name='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` = %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_or_having_null(self):
        sql, params = self.get_recordset().select('*').or_having(id=[1, 2, 3], name=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_or_having_not_between(self):
        sql, params = self.get_recordset().select('*').having(name='Ryan').or_having(id__not_bt=[1, 2])._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` NOT BETWEEN %s AND %s', sql)
        self.assertEqual(['Ryan', 1, 2], params)

    def test_or_having_not(self):
        sql, params = self.get_recordset().select('*').or_having(id__not=1, name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` <> %s OR `name` <> %s', sql)
        self.assertEqual([1, 'Ryan'], params)

    def test_or_having_not_null(self):
        sql, params = self.get_recordset().select('*').or_having(id=[1, 2, 3]).or_having(name__not=None)._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NOT NULL', sql)
        self.assertEqual([1, 2, 3], params)

    def test_or_having_not_in(self):
        sql, params = self.get_recordset().select('*').or_having(id__not=[1, 2, 3], name__not='Ryan')._query_sql()
        self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) OR `name` <> %s', sql)
        self.assertEqual([1, 2, 3, 'Ryan'], params)

    def test_limits_and_offsets(self):
        sql, params = self.get_recordset().select('*').offset(5).limit(10)._query_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 5', sql)
        self.assertEqual([], params)

    def test_page(self):
        sql, params = self.get_recordset().select('*').page(2, 15)._query_sql()
        self.assertEqual('SELECT * FROM `users` LIMIT 15 OFFSET 15', sql)
        self.assertEqual([], params)

    def test_join(self):
        sql, params = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="Ryan")\
          .join('cars', "users.id=cars.user_id").where(cars__name='focus')._query_sql()
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s AND `cars`.`name` = %s', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan', 'focus'], params)

    def test_join_without_select(self):
        sql, params = self.get_recordset().where(id=[1,2,3]).or_where(name="Ryan").join('cars', "users.id=cars.user_id").where(cars__name='focus')._query_sql()
        self.assertEqual('SELECT * FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s AND `cars`.`name` = %s', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan', 'focus'], params)

    def test_join_with_function(self):
        def complex(join):
            join.on('users.id=cars.user_id').and_(cars__id=10).or_(users__id=100)
        sql, params = self.get_recordset().join('cars', func=complex)._query_sql()

        self.assertEqual('SELECT * FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` AND `cars`.`id` = %s OR `users`.`id` = %s', sql)
        self.assertEqual([10, 100], params)

    def test_left_join(self):
        sql, params = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="Ryan").left_join('cars', "users.id=cars.user_id")._query_sql()
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan'], params)

    def test_right_join(self):
        sql, params = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="Ryan").right_join('cars', "users.id=cars.user_id")._query_sql()
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` RIGHT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan'], params)

    def test_cross_join(self):
        sql, params = self.get_recordset().select('users.id').select('users.name').select('cars.name').where(id=[1,2,3]).or_where(name="Ryan").cross_join('cars', "users.id=cars.user_id")._query_sql()
        self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` CROSS JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan'], params)

    def test_count(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count()
        self.assertEqual(2, cnt)

    def test_count_with_column(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count('id')
        self.assertEqual(2, cnt)

    def test_count_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count(distinct=True)
        self.assertEqual(2, cnt)

    def test_count_distinct_with_column(self):
        def _(sql, *params):
            self.assertEqual('SELECT COUNT(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2})
        tb = self.get_recordset()
        tb.db.fetchone = _
        cnt = tb.where(name__not='Lily').count('id', True)
        self.assertEqual(2, cnt)

    def test_max(self):
        def _(sql, *params):
            self.assertEqual('SELECT MAX(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 1024})
        tb = self.get_recordset()
        tb.db.fetchone = _
        max_value = tb.where(name__not='Lily').max('id')
        self.assertEqual(1024, max_value)

    def test_max_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT MAX(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 1024})
        tb = self.get_recordset()
        tb.db.fetchone = _
        max_value = tb.where(name__not='Lily').max('id', True)
        self.assertEqual(1024, max_value)

    def test_min(self):
        def _(sql, *params):
            self.assertEqual('SELECT MIN(`id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 1})
        tb = self.get_recordset()
        tb.db.fetchone = _
        min_value = tb.where(name__not='Lily').min('id')
        self.assertEqual(1, min_value)

    def test_min_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT MIN(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 1})
        tb = self.get_recordset()
        tb.db.fetchone = _
        min_value = tb.where(name__not='Lily').min('id', True)
        self.assertEqual(1, min_value)

    def test_avg(self):
        def _(sql, *params):
            self.assertEqual('SELECT AVG(`age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 32})
        tb = self.get_recordset()
        tb.db.fetchone = _
        avg = tb.where(name__not='Lily').avg('age')
        self.assertEqual(32, avg)

    def test_avg_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT AVG(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 32})
        tb = self.get_recordset()
        tb.db.fetchone = _
        avg = tb.where(name__not='Lily').avg('age', True)
        self.assertEqual(32, avg)

    def test_sum(self):
        def _(sql, *params):
            self.assertEqual('SELECT SUM(`age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2048})
        tb = self.get_recordset()
        tb.db.fetchone = _
        sum_value = tb.where(name__not='Lily').sum('age')
        self.assertEqual(2048, sum_value)

    def test_sum_distinct(self):
        def _(sql, *params):
            self.assertEqual('SELECT SUM(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` <> %s', sql)
            self.assertEqual(['Lily'], list(params))
            return mydict({'aggregate': 2048})
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
        sql, params = self.get_recordset().select('users.id').select('cars.name')\
                 .where(id=[1,2,3]).or_where(name="Ryan")\
                 .left_join('cars', "users.id=cars.user_id").read_lock()._query_sql()
        self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s LOCK IN SHARE MODE', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan'], params)

    def test_write_lock(self):
        sql, params = self.get_recordset().select('users.id').select('cars.name')\
                 .where(id=[1,2,3]).or_where(name="Ryan")\
                 .left_join('cars', "users.id=cars.user_id").write_lock()._query_sql()
        self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s FOR UPDATE', sql)
        self.assertEqual([ 1, 2, 3, 'Ryan'], params)

    def test_where_exists(self):
        users = self.get_recordset()
        mobiles = self.get_recordset("mobiles")

        sql, params = users.where(age__lte=20).where_exists(
            mobiles.where(name='iphone')
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', sql)
        self.assertEqual([20, 'iphone'], params)

    def test_multiple_where_exists(self):
        sql, params = self.get_recordset().where_exists(
            self.get_recordset("mobiles").where(name='iphone'),
            self.get_recordset("mobiles").where(name='aphone')
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', sql)
        self.assertEqual(['iphone', 'aphone'], params)

    def test_or_exists(self):
        users = self.get_recordset()
        mobiles = self.get_recordset("mobiles")

        sql, params = users.where(age__lte=20).or_exists(
            mobiles.where(name='iphone')
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', sql)
        self.assertEqual([20, 'iphone'], params)

    def test_multiple_or_exists(self):
        sql, params = self.get_recordset().or_exists(
            self.get_recordset("mobiles").where(name='iphone'),
            self.get_recordset("mobiles").where(name='aphone')
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', sql)
        self.assertEqual(['iphone', 'aphone'], params)

    def test_complex_where_or_exists(self):
        sql, params = self.get_recordset().where(age__lte=20).where_exists(
                    self.get_recordset("mobiles").where(name='iphone'),
                ).or_exists(
                    self.get_recordset("mobiles").where(name='iphone'),
                    self.get_recordset("mobiles").where(name='aphone')
                ).where_exists(
                    self.get_recordset("mobiles").where(name='aphone')
                )._query_sql()
        expected_sql = 'SELECT * FROM `users` WHERE `age` <= %s' \
              ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
              ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)'
        self.assertEqual(expected_sql, sql)
        self.assertEqual([20, 'iphone', 'iphone', 'aphone', 'aphone'], params)

    def test_unions(self):
        sql, params = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        )._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s UNION SELECT * FROM `users` WHERE `id` = %s',
            sql  
        )
        self.assertEqual([1, 2], params)

    def test_union_alls(self):
        sql, params = self.get_recordset().where(id=1).union_all(
            self.get_recordset().where(id=2)
        )._query_sql()
        self.assertEqual('SELECT * FROM `users` WHERE `id` = %s UNION ALL SELECT * FROM `users` WHERE `id` = %s', sql)
        self.assertEqual([1, 2], params)

    def test_multiple_unions(self):
        sql, params = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).union(
            self.get_recordset().where(id=3)
        )._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s',
            sql
        )
        self.assertEqual([1, 2, 3], params)

    def test_multiple_union_alls(self):
        sql, params = self.get_recordset().where(id=1).union_all(
            self.get_recordset().where(id=2)
        ).union_all(
            self.get_recordset().where(id=3)
        )._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION ALL SELECT * FROM `users` WHERE `id` = %s '
            'UNION ALL SELECT * FROM `users` WHERE `id` = %s',
            sql
        )
        self.assertEqual([1, 2, 3], params)

    def test_union_order_by(self):
        sql, params = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).order_by('id', True)._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'ORDER BY `id` DESC',
            sql
        )
        self.assertEqual([1, 2], params)

    def test_union_limits_and_offsets(self):
        sql, params = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).limit(10).offset(20)._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'LIMIT 10 OFFSET 20',
            sql
        )
        self.assertEqual([1, 2], params)

    def test_union_order_by_and_limits_and_offsets(self):
        sql, params = self.get_recordset().where(id=1).union(
            self.get_recordset().where(id=2)
        ).limit(10).offset(20).order_by('id', False)._query_sql()
        self.assertEqual(
            'SELECT * FROM `users` WHERE `id` = %s '
            'UNION SELECT * FROM `users` WHERE `id` = %s '
            'ORDER BY `id` LIMIT 10 OFFSET 20',
            sql
        )
        self.assertEqual([1, 2], params)


if __name__ == '__main__':
    import unittest
    unittest.main()

    