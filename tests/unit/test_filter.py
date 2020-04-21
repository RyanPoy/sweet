#coding: utf8
from sweet.tests import TestCase
from sweet.database.filters import Filter


class FilterTest(TestCase):
    
    def get_filter(self, name, value):
        return Filter.new(name, value, '`', '%s').compile()

    def test_filter(self):
        f = self.get_filter('name', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual(['ryanpoy'], f.params)
        self.assertEqual('=', f.operator)
        self.assertEqual('`name` = %s', f.sql)

    def test_filter_not(self):
        f = self.get_filter('age__not', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual([10], f.params)
        self.assertEqual('<>', f.operator)
        self.assertEqual('`age` <> %s', f.sql)

    def test_filter_is_none(self):
        f = self.get_filter('name', None)
        self.assertEqual('name', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([], f.params)
        self.assertEqual('IS', f.operator)
        self.assertEqual('`name` IS NULL', f.sql)

    def test_filter_is_not_none(self):
        f = self.get_filter('name__not', None)
        self.assertEqual('name', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([], f.params)
        self.assertEqual('IS NOT', f.operator)
        self.assertEqual('`name` IS NOT NULL', f.sql)

    def test_filter_in(self):
        f = self.get_filter('name', ['ryan', 'poy', 'judy'])
        self.assertEqual('name', f.name)
        self.assertEqual(['ryan', 'poy', 'judy'], f.value)
        self.assertEqual(['ryan', 'poy', 'judy'], f.params)
        self.assertEqual('IN', f.operator)
        self.assertEqual('`name` IN (%s, %s, %s)', f.sql)

    def test_filter_not_in(self):
        f = self.get_filter('age__not', [10, 20, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 20, 30], f.value)
        self.assertEqual([10, 20, 30], f.params)
        self.assertEqual('NOT IN', f.operator)
        self.assertEqual('`age` NOT IN (%s, %s, %s)', f.sql)

    def test_filter_like(self):
        f = self.get_filter('name__like', '%ryan%')
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual(['%ryan%'], f.params)
        self.assertEqual('LIKE', f.operator)
        self.assertEqual('`name` LIKE %s', f.sql)

    def test_filter_not_like(self):
        f = self.get_filter('name__not_like', '%ryan%')
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual(['%ryan%'], f.params)
        self.assertEqual('NOT LIKE', f.operator)
        self.assertEqual('`name` NOT LIKE %s', f.sql)

    def test_filter_between(self):
        f = self.get_filter('age__bt', [10, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual([10, 30], f.params)
        self.assertEqual('BETWEEN', f.operator)
        self.assertEqual('`age` BETWEEN %s AND %s', f.sql)

    def test_filter_not_between(self):
        f = self.get_filter('age__not_bt', [10, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual([10, 30], f.params)
        self.assertEqual('NOT BETWEEN', f.operator)
        self.assertEqual('`age` NOT BETWEEN %s AND %s', f.sql)

    def test_filter_between_should_give_me_error_if_not_a_array_which_has_2_elements(self):
        self.assertRaises(TypeError, self.get_filter, 'age__bt', [10, 20, 30])
        self.assertRaises(TypeError, self.get_filter, 'age__bt', 'abc')

    def test_filter_gt(self):
        f = self.get_filter('name__gt', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual(['ryanpoy'], f.params)
        self.assertEqual('>', f.operator)
        self.assertEqual('`name` > %s', f.sql)

    def test_filter_gte(self):
        f = self.get_filter('name__gte', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual(['ryanpoy'], f.params)
        self.assertEqual('>=', f.operator)
        self.assertEqual('`name` >= %s', f.sql)

    def test_filter_lt(self):
        f = self.get_filter('name__lt', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual(['ryanpoy'], f.params)
        self.assertEqual('<', f.operator)
        self.assertEqual('`name` < %s', f.sql)

    def test_filter_lte(self):
        f = self.get_filter('name__lte', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual(['ryanpoy'], f.params)
        self.assertEqual('<=', f.operator)
        self.assertEqual('`name` <= %s', f.sql)

    def test_filter_is_gt_null(self):
        f = self.get_filter('age__gt', None)
        self.assertEqual('age', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([None], f.params)
        self.assertEqual('>', f.operator)
        self.assertEqual('`age` > %s', f.sql)


if __name__ == '__main__':
    import unittest
    unittest.main()
