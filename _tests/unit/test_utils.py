#coding: utf8
from sweet._tests import TestCase
from sweet.utils import *


class TestUtils(TestCase):

    def test_to_i(self):
        self.assertEqual(9, to_i(9))
        self.assertEqual(9, to_i('9'))
        self.assertEqual(0, to_i('9p'))

    def test_to_f(self):
        self.assertEqual(9.0, to_f(9))
        self.assertEqual(9.0, to_f('9'))
        self.assertEqual(0.0, to_f('9p'))

    def test_to_decimal(self):
        d = Decimal(1)
        self.assertEqual(d, to_decimal(d))
        self.assertEqual(Decimal(9), to_decimal(9))
        self.assertEqual(Decimal(0), to_decimal('9c'))

    def test_to_bool_true(self):
        self.assertTrue(to_bool('abc'))
        self.assertTrue(to_bool(1))
        self.assertTrue(to_bool(True))
        self.assertTrue(to_bool('T'))
        self.assertTrue(to_bool('t'))
        self.assertTrue(to_bool('true'))
        self.assertTrue(to_bool('True'))
        self.assertTrue(to_bool({'A': 'A'}))
        self.assertTrue(to_bool([1]))
        self.assertTrue(to_bool((2)))
        self.assertTrue(to_bool('yes'))
        self.assertTrue(to_bool('Yes'))
        self.assertTrue(to_bool('YES'))

    def test_to_bool_false(self):
        self.assertFalse(to_bool(''))
        self.assertFalse(to_bool('   '))
        self.assertFalse(to_bool(0))
        self.assertFalse(to_bool(False))
        self.assertFalse(to_bool('F'))
        self.assertFalse(to_bool('f'))
        self.assertFalse(to_bool('false'))
        self.assertFalse(to_bool('FALSE'))
        self.assertFalse(to_bool({}))
        self.assertFalse(to_bool([]))
        self.assertFalse(to_bool(()))
        self.assertFalse(to_bool('no'))
        self.assertFalse(to_bool('No'))
        self.assertFalse(to_bool('NO'))

    def test_str2date(self):
        self.assertEqual(date(2009, 10, 10), str2date('2009-10-10'))
        self.assertEqual(date(2009, 10, 10), str2date('2009-10-10 10:10:10'))

    def test_str2datetime(self):
        self.assertEqual(datetime(2009, 10, 10, 10, 10, 10, 0), str2datetime('2009-10-10 10:10:10'))
        self.assertEqual(datetime(2009, 10, 10, 10, 10, 10, 100000), str2datetime('2009-10-10 10:10:10.100000'))
        self.assertEqual(datetime(2009, 10, 10, 10, 10, 10, 100), str2datetime('2009-10-10 10:10:10.000100'))
        self.assertEqual(datetime(2009, 10, 10, 0, 0, 0, 0), str2datetime('2009-10-10'))

    def test_str2date_when_empty_string(self):
        self.assertEqual(None, str2date(''))
        self.assertEqual(None, str2date('  '))
        self.assertEqual(None, str2date(None))


if __name__ == '__main__':
    import unittest
    unittest.main()
