#coding: utf8
import unittest
from sweet.utils.validation import ConfirmationValidator


class TestValidatorConfirmation(unittest.TestCase):

    def test_confirmation(self):
        self.assertTrue(ConfirmationValidator().validate(10, 10))  

    def test_disconfirmation(self):
        self.assertFalse(ConfirmationValidator().validate(10, 100))


if __name__ == '__main__':
    unittest.main()
