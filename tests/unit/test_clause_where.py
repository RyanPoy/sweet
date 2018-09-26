#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.clauses import WhereClause


class WhereClauseTest(TestCase):

    def get_clause(self):
        return WhereClause('`', '%s')

    def test_basic_where_clause(self):
        c = self.get_clause().compile()
        self.assertEqual('', c.sql)
        self.assertEqual([], c.bindings)

    def test_where_and(self):
        c = self.get_clause()
        c.and_(id=1).and_(name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_muliple_kv(self):
        c = self.get_clause()
        c.and_(id=1, name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_not(self):
        c = self.get_clause()
        c.and_(id__not=1, name__not='ryanpoy').compile()
        self.assertEqual('WHERE `id` != %s AND `name` != %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_in(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` = %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_where_and_not_in(self):
        c = self.get_clause()
        c.and_(id__not=[1, 2, 3], name__not='ryanpoy').compile()
        self.assertEqual('WHERE `id` NOT IN (%s, %s, %s) AND `name` != %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_where_and_like(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], name__like='%yanpo%').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` LIKE %s', c.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], c.bindings)

    def test_where_and_not_like(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], name__not_like='%yanpo%').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` NOT LIKE %s', c.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], c.bindings)

    def test_where_and_in_empty(self):
        c = self.get_clause()
        c.and_(id=[], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN () AND `name` = %s', c.sql)
        self.assertEqual(['ryanpoy'], c.bindings)

    def test_where_and_in_none(self):
        c = self.get_clause()
        c.and_(id=[None, None], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN (%s, %s) AND `name` = %s', c.sql)
        self.assertEqual([None, None, 'ryanpoy'], c.bindings)

    def test_where_and_null(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], name=None).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` IS %s', c.sql)
        self.assertEqual([1, 2, 3, None], c.bindings)

    def test_where_and_not_null(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3]).and_(name__not=None).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` IS NOT %s', c.sql)
        self.assertEqual([1, 2, 3, None], c.bindings)

    def test_where_and_gt(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], age__gt=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` > %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_gte(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], age__gte=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` >= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_lt(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], age__lt=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` < %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_lte(self):
        c = self.get_clause()
        c.and_(id=[1, 2, 3], age__lte=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` <= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_between(self):
        c = self.get_clause().and_(id__bt=[1, 2]).compile()
        self.assertEqual('WHERE `id` BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_where_and_not_between(self):
        c = self.get_clause().and_(id__not_bt=[1, 2]).compile()
        self.assertEqual('WHERE `id` NOT BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_where_and_between_should_give_me_error(self):
        c = self.get_clause().and_(id__bt=[1, 2, 3])
        self.assertRaises(TypeError, c.compile)

    def test_or(self):
        c = self.get_clause().or_(id=1, name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s OR `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    # def test_order_by(self):
    #     c = self.get_clause().select('*').order_by('email').order_by('age', 'desc')
    #     self.assertEqual('SELECT * FROM `users` ORDER BY `email`, `age` DESC', c.sql)

    # def test_group_bys(self):
    #     c = self.get_clause().select('*').group_by('id', 'email')
    #     self.assertEqual('SELECT * FROM `users` GROUP BY `id`, `email`', c.sql )

    # def test_having(self):
    #     c = self.get_clause().select('*').having(id=1, name='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` = %s AND `name` = %s', c.sql)
    #     self.assertEqual([1, 'ryanpoy'], c.bindings)

    # def test_having_in(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], name='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` = %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_having_null(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], name=None)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NULL', c.sql)
    #     self.assertEqual([1, 2, 3], c.bindings)

    # def test_having_gt(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], age__gt=30)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` > %s', c.sql)
    #     self.assertEqual([1, 2, 3, 30], c.bindings)

    # def test_having_gte(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], age__gte=30)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` >= %s', c.sql)
    #     self.assertEqual([1, 2, 3, 30], c.bindings)

    # def test_having_lt(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], age__lt=30)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` < %s', c.sql)
    #     self.assertEqual([1, 2, 3, 30], c.bindings)

    # def test_having_lte(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3], age__lte=30)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `age` <= %s', c.sql)
    #     self.assertEqual([1, 2, 3, 30], c.bindings)

    # def test_having_between(self):
    #     c = self.get_clause().select('*').having(id__bt=[1, 2])
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` BETWEEN %s AND %s', c.sql)
    #     self.assertEqual([1, 2], c.bindings)

    # def test_having_between_should_give_me_error(self):
    #     c = self.get_clause().select('*').having(id__bt=[1, 2, 3])
    #     with self.assertRaises(TypeError):
    #         c.sql

    # def test_having_not(self):
    #     c = self.get_clause().select('*').having(id__not=1, name__not='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` != %s AND `name` != %s', c.sql)
    #     self.assertEqual([1, 'ryanpoy'], c.bindings)

    # def test_having_not_null(self):
    #     c = self.get_clause().select('*').having(id=[1, 2, 3]).having(name__not=None)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) AND `name` IS NOT NULL', c.sql)
    #     self.assertEqual([1, 2, 3], c.bindings)

    # def test_having_not_in(self):
    #     c = self.get_clause().select('*').having(id__not=[1, 2, 3], name__not='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) AND `name` != %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_having_not_between(self):
    #     c = self.get_clause().select('*').having(id__not_bt=[1, 2])
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` NOT BETWEEN %s AND %s', c.sql)
    #     self.assertEqual([1, 2], c.bindings)

    # def test_or_having(self):
    #     c = self.get_clause().select('*').or_having(id=1, name='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` = %s OR `name` = %s', c.sql)
    #     self.assertEqual([1, 'ryanpoy'], c.bindings)

    # def test_or_having_between(self):
    #     c = self.get_clause().select('*').having(name='ryanpoy').or_having(id__bt=[1, 2])
    #     self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` BETWEEN %s AND %s', c.sql)
    #     self.assertEqual(['ryanpoy', 1, 2], c.bindings)

    # def test_or_having_in(self):
    #     c = self.get_clause().select('*').or_having(id=[1, 2, 3], name='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` = %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_or_having_null(self):
    #     c = self.get_clause().select('*').or_having(id=[1, 2, 3], name=None)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NULL', c.sql)
    #     self.assertEqual([1, 2, 3], c.bindings)

    # def test_or_having_not_between(self):
    #     c = self.get_clause().select('*').having(name='ryanpoy').or_having(id__not_bt=[1, 2])
    #     self.assertEqual('SELECT * FROM `users` HAVING `name` = %s OR `id` NOT BETWEEN %s AND %s', c.sql)
    #     self.assertEqual(['ryanpoy', 1, 2], c.bindings)

    # def test_or_having_not(self):
    #     c = self.get_clause().select('*').or_having(id__not=1, name__not='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` != %s OR `name` != %s', c.sql)
    #     self.assertEqual([1, 'ryanpoy'], c.bindings)

    # def test_or_having_not_null(self):
    #     c = self.get_clause().select('*').or_having(id=[1, 2, 3]).or_having(name__not=None)
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` IN (%s, %s, %s) OR `name` IS NOT NULL', c.sql)
    #     self.assertEqual([1, 2, 3], c.bindings)

    # def test_or_having_not_in(self):
    #     c = self.get_clause().select('*').or_having(id__not=[1, 2, 3], name__not='ryanpoy')
    #     self.assertEqual('SELECT * FROM `users` HAVING `id` NOT IN (%s, %s, %s) OR `name` != %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_limits_and_offsets(self):
    #     c = self.get_clause().select('*').offset(5).limit(10)
    #     self.assertEqual('SELECT * FROM `users` LIMIT 10 OFFSET 5', c.sql)

    # def test_page(self):
    #     c = self.get_clause().select('*').page(2, 15)
    #     self.assertEqual('SELECT * FROM `users` LIMIT 15 OFFSET 15', c.sql)

    # def test_join(self):
    #     c = self.get_clause().select('users.id').select('users.name').select('cars.name').and_(id=[1,2,3]).or_(name="ryanpoy")\
    #       .join('cars', on="users.id=cars.user_id").and_(cars__name='focus')
    #     self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` INNER JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s AND `cars`.`name` = %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy', 'focus'], c.bindings)

    # def test_left_join(self):
    #     c = self.get_clause().select('users.id').select('users.name').select('cars.name').and_(id=[1,2,3]).or_(name="ryanpoy").left_join('cars', on="users.id=cars.user_id")
    #     self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_right_join(self):
    #     c = self.get_clause().select('users.id').select('users.name').select('cars.name').and_(id=[1,2,3]).or_(name="ryanpoy").right_join('cars', on="users.id=cars.user_id")
    #     self.assertEqual('SELECT `users`.`id`, `users`.`name`, `cars`.`name` FROM `users` RIGHT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_count(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     cnt = c.and_(name__not='Lily').count()
    #     self.assertEqual(2, cnt)

    # def test_count_with_column(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT COUNT(`id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     cnt = c.and_(name__not='Lily').count('id')
    #     self.assertEqual(2, cnt)

    # def test_count_distinct(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT COUNT(*) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     cnt = c.and_(name__not='Lily').count(distinct=True)
    #     self.assertEqual(2, cnt)

    # def test_count_distinct_with_column(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT COUNT(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     cnt = c.and_(name__not='Lily').count('id', True)
    #     self.assertEqual(2, cnt)

    # def test_max(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT MAX(`id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 1024})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     max_value = c.and_(name__not='Lily').max('id')
    #     self.assertEqual(1024, max_value)

    # def test_max_distinct(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT MAX(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 1024})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     max_value = c.and_(name__not='Lily').max('id', True)
    #     self.assertEqual(1024, max_value)

    # def test_min(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT MIN(`id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 1})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     min_value = c.and_(name__not='Lily').min('id')
    #     self.assertEqual(1, min_value)

    # def test_min_distinct(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT MIN(DISTINCT `id`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 1})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     min_value = c.and_(name__not='Lily').min('id', True)
    #     self.assertEqual(1, min_value)

    # def test_avg(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT AVERAGE(`age`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 32})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     avg = c.and_(name__not='Lily').avg('age')
    #     self.assertEqual(32, avg)

    # def test_avg_distinct(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT AVERAGE(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 32})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     avg = c.and_(name__not='Lily').avg('age', True)
    #     self.assertEqual(32, avg)

    # def test_sum(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT SUM(`age`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2048})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     sum_value = c.and_(name__not='Lily').sum('age')
    #     self.assertEqual(2048, sum_value)

    # def test_sum_distinct(self):
    #     def _(sql, *params):
    #         self.assertEqual('SELECT SUM(DISTINCT `age`) AS aggregate FROM `users` WHERE `name` != %s', sql)
    #         self.assertEqual(['Lily'], list(params))
    #         return Record({'aggregate': 2048})
    #     c = self.get_clause()
    #     c.db.fetchone = _
    #     sum_value = c.and_(name__not='Lily').sum('age', True)
    #     self.assertEqual(2048, sum_value)

    # def test_exists_distinct(self):
    #     c = self.get_clause()
    #     c.first = lambda: {'id': 1, 'name': 'Poy', 'age': 25}
    #     exists = c.and_(name__not='Lily').exists()
    #     self.assertEqual(True, exists)
    
    # def test_read_lock(self):
    #     c = self.get_clause().select('users.id').select('cars.name')\
    #              .and_(id=[1,2,3]).or_(name="ryanpoy")\
    #              .left_join('cars', on="users.id=cars.user_id").read_lock()
    #     self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s LOCK IN SHARE MODE', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_write_lock(self):
    #     c = self.get_clause().select('users.id').select('cars.name')\
    #              .and_(id=[1,2,3]).or_(name="ryanpoy")\
    #              .left_join('cars', on="users.id=cars.user_id").write_lock()
    #     self.assertEqual('SELECT `users`.`id`, `cars`.`name` FROM `users` LEFT JOIN `cars` ON `users`.`id` = `cars`.`user_id` WHERE `id` IN (%s, %s, %s) OR `name` = %s FOR UPDATE', c.sql)
    #     self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    # def test_where_and_exists(self):
    #     users = self.get_table()
    #     mobiles = self.get_table("mobiles")

    #     users = users.and_(age__lte=20).where_exists(
    #         mobiles.and_(name='iphone')
    #     )
    #     self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
    #     self.assertEqual([20, 'iphone'], users.bindings)

    # def test_multiple_where_exists(self):
    #     users = self.get_table().where_exists(
    #         self.get_table("mobiles").and_(name='iphone'),
    #         self.get_table("mobiles").and_(name='aphone')
    #     )
    #     self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
    #     self.assertEqual(['iphone', 'aphone'], users.bindings)

    # def test_or_exists(self):
    #     users = self.get_table()
    #     mobiles = self.get_table("mobiles")

    #     users = users.and_(age__lte=20).or_exists(
    #         mobiles.and_(name='iphone')
    #     )
    #     self.assertEqual('SELECT * FROM `users` WHERE `age` <= %s OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
    #     self.assertEqual([20, 'iphone'], users.bindings)

    # def test_multiple_or_exists(self):
    #     users = self.get_table().or_exists(
    #         self.get_table("mobiles").and_(name='iphone'),
    #         self.get_table("mobiles").and_(name='aphone')
    #     )
    #     self.assertEqual('SELECT * FROM `users` WHERE EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s) OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)', users.sql)
    #     self.assertEqual(['iphone', 'aphone'], users.bindings)

    # def test_complex_where_or_exists(self):
    #     users = self.get_table().and_(age__lte=20).where_exists(
    #                 self.get_table("mobiles").and_(name='iphone'),
    #             ).or_exists(
    #                 self.get_table("mobiles").and_(name='iphone'),
    #                 self.get_table("mobiles").and_(name='aphone')
    #             ).where_exists(
    #                 self.get_table("mobiles").and_(name='aphone')
    #             )
    #     sql = 'SELECT * FROM `users` WHERE `age` <= %s' \
    #           ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
    #           ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
    #           ' OR EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)' \
    #           ' AND EXISTS (SELECT * FROM `mobiles` WHERE `name` = %s)'
    #     self.assertEqual(sql, users.sql)
    #     self.assertEqual([20, 'iphone', 'iphone', 'aphone', 'aphone'], users.bindings)


if __name__ == '__main__':
    import unittest
    unittest.main()

    