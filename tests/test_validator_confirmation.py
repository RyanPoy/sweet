#coding: utf8

from ..validation import ConfirmationValidator
import unittest


class ConfirmationValidatorTest(unittest.TestCase):

    def test_confirmation(self):
        self.assertTrue(ConfirmationValidator().validate(10, 10))  

    def test_disconfirmation(self):
        self.assertFalse(ConfirmationValidator().validate(10, 100))


if __name__ == '__main__':
    unittest.main()
