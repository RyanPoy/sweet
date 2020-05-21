#coding: utf8
from sweet.utils.validation import AcceptanceValidator
from sweet._tests import TestCase


class TestValidatorAcceptance(TestCase):
    
    def test_acceptance(self):
        self.assertTrue(AcceptanceValidator().validate(10))  

    def test_acceptance_should_return_false_if_disallow_null(self):
        self.assertFalse(AcceptanceValidator().validate(None, allow_null=False))

    def test_acceptance_should_return_true_if_allow_null(self):
        self.assertTrue(AcceptanceValidator().validate(None))


if __name__ == '__main__':
    import unittest
    unittest.main()
