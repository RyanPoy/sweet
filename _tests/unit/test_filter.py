#coding: utf8
from sweet._tests import TestCase
from sweet.db.filters import Filter


class TestFilter(TestCase):
    
    def get_filter(self, name, value):
        return Filter.new(name, value, '`', '%s')

    def test_filter(self):
        f = self.get_filter('name', 'Ryan')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('Ryan', f.value)
        self.assertEqual(['Ryan'], params)
        self.assertEqual('=', f.operator)
        self.assertEqual('`name` = %s', sql)

    def test_filter_not(self):
        f = self.get_filter('age__not', 10)
        sql, params = f.compile()
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual([10], params)
        self.assertEqual('<>', f.operator)
        self.assertEqual('`age` <> %s', sql)

    def test_filter_is_none(self):
        f = self.get_filter('name', None)
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([], params)
        self.assertEqual('IS', f.operator)
        self.assertEqual('`name` IS NULL', sql)

    def test_filter_is_not_none(self):
        f = self.get_filter('name__not', None)
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([], params)
        self.assertEqual('IS NOT', f.operator)
        self.assertEqual('`name` IS NOT NULL', sql)

    def test_filter_in(self):
        f = self.get_filter('name', ['ryan', 'poy', 'judy'])
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual(['ryan', 'poy', 'judy'], f.value)
        self.assertEqual(['ryan', 'poy', 'judy'], params)
        self.assertEqual('IN', f.operator)
        self.assertEqual('`name` IN (%s, %s, %s)', sql)

    def test_filter_not_in(self):
        f = self.get_filter('age__not', [10, 20, 30])
        sql, params = f.compile()
        self.assertEqual('age', f.name)
        self.assertEqual([10, 20, 30], f.value)
        self.assertEqual([10, 20, 30], params)
        self.assertEqual('NOT IN', f.operator)
        self.assertEqual('`age` NOT IN (%s, %s, %s)', sql)

    def test_filter_like(self):
        f = self.get_filter('name__like', '%ryan%')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual(['%ryan%'], params)
        self.assertEqual('LIKE', f.operator)
        self.assertEqual('`name` LIKE %s', sql)

    def test_filter_not_like(self):
        f = self.get_filter('name__not_like', '%ryan%')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual(['%ryan%'], params)
        self.assertEqual('NOT LIKE', f.operator)
        self.assertEqual('`name` NOT LIKE %s', sql)

    def test_filter_between(self):
        f = self.get_filter('age__bt', [10, 30])
        sql, params = f.compile()
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual([10, 30], params)
        self.assertEqual('BETWEEN', f.operator)
        self.assertEqual('`age` BETWEEN %s AND %s', sql)

    def test_filter_not_between(self):
        f = self.get_filter('age__not_bt', [10, 30])
        sql, params = f.compile()
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual([10, 30], params)
        self.assertEqual('NOT BETWEEN', f.operator)
        self.assertEqual('`age` NOT BETWEEN %s AND %s', sql)

    def test_filter_between_should_give_me_error_if_not_a_array_which_has_2_elements(self):
        self.assertRaises(TypeError, self.get_filter, 'age__bt', [10, 20, 30])
        self.assertRaises(TypeError, self.get_filter, 'age__bt', 'abc')

    def test_filter_gt(self):
        f = self.get_filter('name__gt', 'Ryan')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('Ryan', f.value)
        self.assertEqual(['Ryan'], params)
        self.assertEqual('>', f.operator)
        self.assertEqual('`name` > %s', sql)

    def test_filter_gte(self):
        f = self.get_filter('name__gte', 'Ryan')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('Ryan', f.value)
        self.assertEqual(['Ryan'], params)
        self.assertEqual('>=', f.operator)
        self.assertEqual('`name` >= %s', sql)

    def test_filter_lt(self):
        f = self.get_filter('name__lt', 'Ryan')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('Ryan', f.value)
        self.assertEqual(['Ryan'], params)
        self.assertEqual('<', f.operator)
        self.assertEqual('`name` < %s', sql)

    def test_filter_lte(self):
        f = self.get_filter('name__lte', 'Ryan')
        sql, params = f.compile()
        self.assertEqual('name', f.name)
        self.assertEqual('Ryan', f.value)
        self.assertEqual(['Ryan'], params)
        self.assertEqual('<=', f.operator)
        self.assertEqual('`name` <= %s', sql)

    def test_filter_is_gt_null(self):
        f = self.get_filter('age__gt', None)
        sql, params = f.compile()
        self.assertEqual('age', f.name)
        self.assertEqual(None, f.value)
        self.assertEqual([None], params)
        self.assertEqual('>', f.operator)
        self.assertEqual('`age` > %s', sql)


if __name__ == '__main__':
    import unittest
    unittest.main()
