#coding: utf8
from sweet.tests import TestCase
from sweet.database.clauses import WhereClause, HavingClause, \
                                    JoinClause, LeftJoinClause, \
                                    RightJoinClause, OrderClause, \
                                    GroupClause, PageClause, \
                                    SelectClause


class ClausesTest(TestCase):

    def setUp(self):
        self.where_clause = WhereClause('`', '%s')
        self.having_clause = HavingClause('`', '%s')
        self.inner_join_clause = JoinClause('`', '%s', 'mobiles')
        self.left_join_clause = LeftJoinClause('`', '%s', 'mobiles')
        self.right_join_clause = RightJoinClause('`', '%s', 'mobiles')
        self.order_clause = OrderClause('`')
        self.group_clause = GroupClause('`')
        self.select_clause = SelectClause('`')
        self.page_clause = PageClause()

    def test_basic_where_clause(self):
        c = self.where_clause.compile()
        self.assertEqual('', c.sql)
        self.assertEqual([], c.bindings)

    def test_where_and(self):
        c = self.where_clause
        c.and_(id=1).and_(name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_muliple_kv(self):
        c = self.where_clause
        c.and_(id=1, name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_not(self):
        c = self.where_clause
        c.and_(id__not=1, name__not='ryanpoy').compile()
        self.assertEqual('WHERE `id` <> %s AND `name` <> %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_where_and_in(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` = %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_where_and_not_in(self):
        c = self.where_clause
        c.and_(id__not=[1, 2, 3], name__not='ryanpoy').compile()
        self.assertEqual('WHERE `id` NOT IN (%s, %s, %s) AND `name` <> %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_where_and_like(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], name__like='%yanpo%').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` LIKE %s', c.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], c.bindings)

    def test_where_and_not_like(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], name__not_like='%yanpo%').compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` NOT LIKE %s', c.sql)
        self.assertEqual([1, 2, 3, '%yanpo%'], c.bindings)

    def test_where_and_in_empty(self):
        c = self.where_clause
        c.and_(id=[], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN () AND `name` = %s', c.sql)
        self.assertEqual(['ryanpoy'], c.bindings)

    def test_where_and_in_none(self):
        c = self.where_clause
        c.and_(id=[None, None], name='ryanpoy').compile()
        self.assertEqual('WHERE `id` IN (%s, %s) AND `name` = %s', c.sql)
        self.assertEqual([None, None, 'ryanpoy'], c.bindings)

    def test_where_and_null(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], name=None).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` IS NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_where_and_not_null(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3]).and_(name__not=None).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `name` IS NOT NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_where_and_gt(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], age__gt=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` > %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_gte(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], age__gte=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` >= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_lt(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], age__lt=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` < %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_lte(self):
        c = self.where_clause
        c.and_(id=[1, 2, 3], age__lte=30).compile()
        self.assertEqual('WHERE `id` IN (%s, %s, %s) AND `age` <= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_where_and_between(self):
        c = self.where_clause.and_(id__bt=[1, 2]).compile()
        self.assertEqual('WHERE `id` BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_where_and_not_between(self):
        c = self.where_clause.and_(id__not_bt=[1, 2]).compile()
        self.assertEqual('WHERE `id` NOT BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_where_and_between_should_give_me_error(self):
        self.assertRaises(TypeError, self.where_clause.and_, id__bt=[1, 2, 3])

    def test_where_or(self):
        c = self.where_clause.or_(id=1, name='ryanpoy').compile()
        self.assertEqual('WHERE `id` = %s OR `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_basic_having_clause(self):
        c = self.having_clause.compile()
        self.assertEqual('', c.sql)
        self.assertEqual([], c.bindings)

    def test_having_and(self):
        c = self.having_clause
        c.and_(id=1).and_(name='ryanpoy').compile()
        self.assertEqual('HAVING `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_having_or(self):
        c = self.having_clause.or_(id=1, name='ryanpoy').compile()
        self.assertEqual('HAVING `id` = %s OR `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_order_by(self):
        c = self.order_clause.by('email').by('age', desc=True).compile()
        self.assertEqual('ORDER BY `email`, `age` DESC', c.sql)

    def test_group_bys(self):
        c = self.group_clause.by('id', 'email').compile()
        self.assertEqual('GROUP BY `id`, `email`', c.sql )

    def test_having(self):
        c = self.having_clause.and_(id=1, name='ryanpoy').compile()
        self.assertEqual('HAVING `id` = %s AND `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_having_in(self):
        c = self.having_clause.and_(id=[1, 2, 3], name='ryanpoy').compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `name` = %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_having_null(self):
        c = self.having_clause.and_(id=[1, 2, 3], name=None).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `name` IS NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_having_gt(self):
        c = self.having_clause.and_(id=[1, 2, 3], age__gt=30).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `age` > %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_having_gte(self):
        c = self.having_clause.and_(id=[1, 2, 3], age__gte=30).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `age` >= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_having_lt(self):
        c = self.having_clause.and_(id=[1, 2, 3], age__lt=30).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `age` < %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_having_lte(self):
        c = self.having_clause.and_(id=[1, 2, 3], age__lte=30).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `age` <= %s', c.sql)
        self.assertEqual([1, 2, 3, 30], c.bindings)

    def test_having_between(self):
        c = self.having_clause.and_(id__bt=[1, 2]).compile()
        self.assertEqual('HAVING `id` BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_having_between_should_give_me_error(self):
        self.assertRaises(TypeError, self.having_clause.and_, id__bt=[1, 2, 3])

    def test_having_not(self):
        c = self.having_clause.and_(id__not=1, name__not='ryanpoy').compile()
        self.assertEqual('HAVING `id` <> %s AND `name` <> %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_having_not_null(self):
        c = self.having_clause.and_(id=[1, 2, 3]).and_(name__not=None).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) AND `name` IS NOT NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_having_not_in(self):
        c = self.having_clause.and_(id__not=[1, 2, 3], name__not='ryanpoy').compile()
        self.assertEqual('HAVING `id` NOT IN (%s, %s, %s) AND `name` <> %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_having_not_between(self):
        c = self.having_clause.and_(id__not_bt=[1, 2]).compile()
        self.assertEqual('HAVING `id` NOT BETWEEN %s AND %s', c.sql)
        self.assertEqual([1, 2], c.bindings)

    def test_or_having(self):
        c = self.having_clause.or_(id=1, name='ryanpoy').compile()
        self.assertEqual('HAVING `id` = %s OR `name` = %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_or_having_between(self):
        c = self.having_clause.and_(name='ryanpoy').or_(id__bt=[1, 2]).compile()
        self.assertEqual('HAVING `name` = %s OR `id` BETWEEN %s AND %s', c.sql)
        self.assertEqual(['ryanpoy', 1, 2], c.bindings)

    def test_or_having_in(self):
        c = self.having_clause.or_(id=[1, 2, 3], name='ryanpoy').compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) OR `name` = %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_or_having_null(self):
        c = self.having_clause.or_(id=[1, 2, 3], name=None).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) OR `name` IS NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_or_having_not_between(self):
        c = self.having_clause.and_(name='ryanpoy').or_(id__not_bt=[1, 2]).compile()
        self.assertEqual('HAVING `name` = %s OR `id` NOT BETWEEN %s AND %s', c.sql)
        self.assertEqual(['ryanpoy', 1, 2], c.bindings)

    def test_or_having_not(self):
        c = self.having_clause.or_(id__not=1, name__not='ryanpoy').compile()
        self.assertEqual('HAVING `id` <> %s OR `name` <> %s', c.sql)
        self.assertEqual([1, 'ryanpoy'], c.bindings)

    def test_or_having_not_null(self):
        c = self.having_clause.or_(id=[1, 2, 3]).or_(name__not=None).compile()
        self.assertEqual('HAVING `id` IN (%s, %s, %s) OR `name` IS NOT NULL', c.sql)
        self.assertEqual([1, 2, 3], c.bindings)

    def test_or_having_not_in(self):
        c = self.having_clause.or_(id__not=[1, 2, 3], name__not='ryanpoy').compile()
        self.assertEqual('HAVING `id` NOT IN (%s, %s, %s) OR `name` <> %s', c.sql)
        self.assertEqual([1, 2, 3, 'ryanpoy'], c.bindings)

    def test_limits_and_offsets(self):
        c = self.page_clause.offset(5).limit(10).compile()
        self.assertEqual('LIMIT 10 OFFSET 5', c.sql)

    def test_page(self):
        c = self.page_clause.page(2, 15).compile()
        self.assertEqual('LIMIT 15 OFFSET 15', c.sql)

    def test_select(self):
        c = self.select_clause.compile()
        self.assertEqual('SELECT *', c.sql)

    def test_select_with_columns(self):
        c = self.select_clause.select('name').select('users.age').compile()
        self.assertEqual('SELECT `name`, `users`.`age`', c.sql)

    def test_select_with_distinct(self):
        c = self.select_clause.distinct().compile()
        self.assertEqual('SELECT DISTINCT *', c.sql)

    def test_select_with_distinct_and_columns(self):
        c = self.select_clause.select('name').select('users.age').distinct().compile()
        self.assertEqual('SELECT DISTINCT `name`, `users`.`age`', c.sql)

    def test_join_and(self):
        c = self.inner_join_clause
        c.on('users.id=cars.user_id').and_(mobiles__name='iphone').and_(users__age=20).compile()
        self.assertEqual('INNER JOIN `mobiles` ON `users`.`id` = `cars`.`user_id` AND `mobiles`.`name` = %s AND `users`.`age` = %s', c.sql)
        self.assertEqual(['iphone', 20], c.bindings)

    def test_join_or(self):
        c = self.inner_join_clause
        c.on('users.id=cars.user_id').or_(mobiles__name='iphone').or_(users__age=20).compile()
        self.assertEqual('INNER JOIN `mobiles` ON `users`.`id` = `cars`.`user_id` OR `mobiles`.`name` = %s OR `users`.`age` = %s', c.sql)
        self.assertEqual(['iphone', 20], c.bindings)

    def test_join_without_on(self):
        c = self.inner_join_clause
        c.or_(mobiles__name='iphone').or_(users__age=20).compile()
        self.assertEqual('INNER JOIN `mobiles` ON `mobiles`.`name` = %s OR `users`.`age` = %s', c.sql)
        self.assertEqual(['iphone', 20], c.bindings)

    def test_left_join(self):
        c = self.left_join_clause
        c.on('users.id=cars.user_id').or_(mobiles__name='iphone').or_(users__age=20).compile()
        self.assertEqual('LEFT JOIN `mobiles` ON `users`.`id` = `cars`.`user_id` OR `mobiles`.`name` = %s OR `users`.`age` = %s', c.sql)
        self.assertEqual(['iphone', 20], c.bindings)

    def test_right_join(self):
        c = self.right_join_clause
        c.on('users.id=cars.user_id').or_(mobiles__name='iphone').or_(users__age=20).compile()
        self.assertEqual('RIGHT JOIN `mobiles` ON `users`.`id` = `cars`.`user_id` OR `mobiles`.`name` = %s OR `users`.`age` = %s', c.sql)
        self.assertEqual(['iphone', 20], c.bindings)


if __name__ == '__main__':
    import unittest
    unittest.main()

    