import unittest
import sweet.database.sql.operator as operator


class TestOperator(unittest.TestCase):

    def test_normal(self):
        self.assertEqual('name = %s', operator.opt('name', 'jim', '%s'))
        self.assertEqual('name <> %s', operator.opt('name__not', 'jim', '%s'))

        self.assertEqual('name IS NULL', operator.opt('name', None, '%s'))
        self.assertEqual('name IS NOT NULL', operator.opt('name__not', None, '%s'))

        self.assertEqual('name IN (%s, %s, %s)', operator.opt('name', ['jim', 'lucy', 'lily'], '%s'))
        self.assertEqual('name NOT IN (%s, %s)', operator.opt('name__not', ['lucy', 'lily'], '%s'))

        self.assertEqual('name LIKE %s', operator.opt('name__like', '%sjim', '%s'))
        self.assertEqual('name NOT LIKE %s', operator.opt('name__not_like', 'lucy%s', '%s'))

        self.assertEqual('age BETWEEN %s AND %s', operator.opt('age__bt', [10, 60], '%s'))
        self.assertEqual('age NOT BETWEEN %s AND %s', operator.opt('age__not_bt', [10, 60], '%s'))

        self.assertEqual('age > %s', operator.opt('age__gt', 20, '%s'))
        self.assertEqual('age >= %s', operator.opt('age__gte', 20, '%s'))
        self.assertEqual('age < %s', operator.opt('age__lt', 20, '%s'))
        self.assertEqual('age <= %s', operator.opt('age__lte', 20, '%s'))

    def test_error(self):
        with self.assertRaises(TypeError) as ctx:
            operator.opt('age__bt', [10, 20, 30], '%s')
        self.assertEqual('The bt or not_bt operation expects a list or tuple of length 2, but it is not.', str(ctx.exception))

        self.assertRaises(TypeError, operator.opt, 'age__not_bt', 'abc', '%s')


if __name__ == '__main__':
    unittest.main()
