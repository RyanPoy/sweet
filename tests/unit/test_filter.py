#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.clauses import Filter


class FilterTest(TestCase):
        
    def test_filter(self):
        f = Filter('name', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual('=', f.operator)
        self.assertEqual('`name` = %s', f.to_sql('`', '%s'))

    def test_filter_not(self):
        f = Filter('age__not', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual('!=', f.operator)
        self.assertEqual('`age` != %s', f.to_sql('`', '%s'))

    def test_filter_in(self):
        f = Filter('name', ['ryan', 'poy', 'judy'])
        self.assertEqual('name', f.name)
        self.assertEqual(['ryan', 'poy', 'judy'], f.value)
        self.assertEqual('IN', f.operator)
        self.assertEqual('`name` IN (%s, %s, %s)', f.to_sql('`', '%s'))

    def test_filter_not_in(self):
        f = Filter('age__not', [10, 20, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 20, 30], f.value)
        self.assertEqual('NOT IN', f.operator)
        self.assertEqual('`age` NOT IN (%s, %s, %s)', f.to_sql('`', '%s'))

    def test_filter_like(self):
        f = Filter('name__like', '%ryan%')
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual('LIKE', f.operator)
        self.assertEqual('`name` LIKE %s', f.to_sql('`', '%s'))

    def test_filter_not_like(self):
        f = Filter('name__not_like', '%ryan%')
        self.assertEqual('name', f.name)
        self.assertEqual('%ryan%', f.value)
        self.assertEqual('NOT LIKE', f.operator)
        self.assertEqual('`name` NOT LIKE %s', f.to_sql('`', '%s'))

    def test_filter_between(self):
        f = Filter('age__bt', [10, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual('BETWEEN', f.operator)
        self.assertEqual('`age` BETWEEN %s AND %s', f.to_sql('`', '%s'))

    def test_filter_not_between(self):
        f = Filter('age__not_bt', [10, 30])
        self.assertEqual('age', f.name)
        self.assertEqual([10, 30], f.value)
        self.assertEqual('NOT BETWEEN', f.operator)
        self.assertEqual('`age` NOT BETWEEN %s AND %s', f.to_sql('`', '%s'))

    def test_filter_between_should_give_me_error_if_not_a_array_which_has_2_elements(self):
        f = Filter('age__bt', [10, 20, 30])
        self.assertRaises(Exception, f.to_sql, '`', '%s')

        f = Filter('age__bt', 'abc')
        self.assertRaises(Exception, f.to_sql, '`', '%s')

    def test_filter_gt(self):
        f = Filter('name__gt', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual('>', f.operator)
        self.assertEqual('`name` > %s', f.to_sql('`', '%s'))

    def test_filter_not_gt(self):
        f = Filter('age__not_gt', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual('<=', f.operator)
        self.assertEqual('`age` <= %s', f.to_sql('`', '%s'))

    def test_filter_gte(self):
        f = Filter('name__gte', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual('>=', f.operator)
        self.assertEqual('`name` >= %s', f.to_sql('`', '%s'))

    def test_filter_not_gte(self):
        f = Filter('age__not_gte', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual('<', f.operator)
        self.assertEqual('`age` < %s', f.to_sql('`', '%s'))

    def test_filter_lt(self):
        f = Filter('name__lt', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual('<', f.operator)
        self.assertEqual('`name` < %s', f.to_sql('`', '%s'))

    def test_filter_not_lt(self):
        f = Filter('age__not_lt', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual('>=', f.operator)
        self.assertEqual('`age` >= %s', f.to_sql('`', '%s'))

    def test_filter_lte(self):
        f = Filter('name__lte', 'ryanpoy')
        self.assertEqual('name', f.name)
        self.assertEqual('ryanpoy', f.value)
        self.assertEqual('<=', f.operator)
        self.assertEqual('`name` <= %s', f.to_sql('`', '%s'))

    def test_filter_not_lte(self):
        f = Filter('age__not_lte', 10)
        self.assertEqual('age', f.name)
        self.assertEqual(10, f.value)
        self.assertEqual('>', f.operator)
        self.assertEqual('`age` > %s', f.to_sql('`', '%s'))


if __name__ == '__main__':
    import unittest
    unittest.main()
