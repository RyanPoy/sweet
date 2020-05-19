# coding: utf8
from sweet._tests import TestCase
from sweet.utils.collection import *

class Foobar(object):
    pass


class TestCollection(TestCase):

    def test_get_value(self):
        obj = Foobar()
        obj.a = 10
        self.assertEqual(None, get_value(obj, 'b'))
        self.assertEqual(10, get_value(obj, 'a'))

    def test_get_value_multi_level(self):
        obj = Foobar()
        obj.a = Foobar()
        obj.a.b = Foobar()
        obj.a.b.v = 10

        self.assertEqual(None, get_value(obj, 'b'))
        self.assertEqual(None, get_value(obj, 'a.c'))
        self.assertEqual(None, get_value(obj, 'a.b.c'))
        self.assertEqual(10, get_value(obj, 'a.b.v'))


if __name__ == '__main__':
    import unittest
    unittest.main()
