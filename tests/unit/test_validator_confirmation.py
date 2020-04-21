#coding: utf8
from sweet.utils.validation import ConfirmationValidator
from sweet.tests import TestCase


class ConfirmationValidatorTest(TestCase):

    def test_confirmation(self):
        self.assertTrue(ConfirmationValidator().validate(10, 10))  

    def test_disconfirmation(self):
        self.assertFalse(ConfirmationValidator().validate(10, 100))


if __name__ == '__main__':
    import unittest
    unittest.main()
