#coding: utf8
from sweet.utils.validation import LengthValidator
from sweet._tests import TestCase


class TestValidatorLength(TestCase):

    def test_length_is_5(self):
        self.assertTrue(LengthValidator().validate('hello', _is = 5))  
        
    def test_length_is_not_5(self):
        self.assertFalse(LengthValidator().validate('hello, world', _is = 5))

    def test_length_minimum_is_5(self):
        self.assertTrue(LengthValidator().validate('hello, world', minimum = 5))
        self.assertTrue(LengthValidator().validate('hello', minimum = 5))

    def test_length_should_return_false_if_length_less_than_minimum(self):
        self.assertFalse(LengthValidator().validate('hi', minimum = 5))
        
    def test_length_maximun_is_10(self):
        self.assertTrue(LengthValidator().validate('hello', maximum = 10))
        self.assertTrue(LengthValidator().validate('diplayport', maximum = 10))
        
    def test_length_should_return_false_if_length_greater_than_maximum(self):
        self.assertFalse(LengthValidator().validate('validations.value', maximum = 10))

    def test_length_should_return_true_if_value_is_null_and_allow_null(self):
        self.assertTrue(LengthValidator().validate(None, allow_null=True))
        
    def test_length_should_return_false_if_value_is_null_and_disallow_null(self):
        self.assertFalse(LengthValidator().validate(None, allow_null=False))

    def test_length_should_return_true_if_value_is_blank_str_and_allow_blank(self):
        self.assertTrue(LengthValidator().validate('     ', allow_blank=True))
        
    def test_length_should_return_false_if_value_is_blank_and_disallow_blank(self):
        self.assertFalse(LengthValidator().validate([], allow_blank=False))
        

if __name__ == '__main__':
    import unittest
    unittest.main()
