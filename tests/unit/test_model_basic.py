#coding: utf8
from sweet.tests import TestCase, User


class ModelBasicTest(TestCase):

    def test_model_columns(self):
        self.assertTrue(['id', 'name', 'age'], User.__columns__)


    # def test_create(self):
    #     u = User(name="ryan", age=32)
    #     u.create()
    #     self.assertTrue(u.id > 0)


if __name__ == '__main__':
    import unittest
    unittest.main()
