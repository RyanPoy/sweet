#coding: utf8
import unittest
from sweet.utils.validation import AcceptanceValidator


class TestValidatorAcceptance(unittest.TestCase):
    
    def test_acceptance(self):
        self.assertTrue(AcceptanceValidator().validate(10))  

    def test_acceptance_should_return_false_if_disallow_null(self):
        self.assertFalse(AcceptanceValidator().validate(None, allow_null=False))

    def test_acceptance_should_return_true_if_allow_null(self):
        self.assertTrue(AcceptanceValidator().validate(None))


if __name__ == '__main__':
    unittest.main()
