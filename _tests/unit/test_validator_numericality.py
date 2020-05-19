#coding: utf8
from sweet.utils.validation import NumericalityValidator
from sweet._tests import TestCase


class TestValidatorNumericality(TestCase):
        
    def test_numericality_should_return_false_if_value_is_null(self):
        self.assertFalse(NumericalityValidator().validate(None, greater_than = 10))
        
    def test_numericality_should_return_false_if_value_is_not_a_number(self):
        self.assertFalse(NumericalityValidator().validate('s', greater_than = 10))
        
    def test_numericality_greate_than(self):
        self.assertTrue(NumericalityValidator().validate(11, greater_than = 10))
        
    def test_numericality_should_return_false_if_value_need_greater_than_10_but_it_equals(self):
        self.assertFalse(NumericalityValidator().validate(10, greater_than = 10))
        
    def test_numericality_should_return_false_if_value_need_greater_than_10_but_it_less_than(self):
        self.assertFalse(NumericalityValidator().validate(9, greater_than = 10))
    
    def test_numericality_great_than_or_equal_to(self):
        self.assertTrue(NumericalityValidator().validate(10, greater_than_or_equal_to = 10))
        self.assertTrue(NumericalityValidator().validate(11, greater_than_or_equal_to = 10))
    
    def test_numericality_should_return_false_if_value_need_greater_than_10_or_equal_but_it_less_than(self):
        self.assertFalse(NumericalityValidator().validate(9, greater_than_or_equal_to = 10))
        
    def test_numericality_equal_to(self):
        self.assertTrue(NumericalityValidator().validate(10, equal_to = 10))
        
    def test_numericality_should_return_false_if_value_need_equal_10_but_it_less_than(self):
        self.assertFalse(NumericalityValidator().validate(9, equal_to = 10))
    
    def test_numericality_should_return_false_if_value_need_equal_10_but_it_greate_than(self):
        self.assertFalse(NumericalityValidator().validate(11, equal_to = 10))

    def test_numericality_less_than(self):
        self.assertTrue(NumericalityValidator().validate(9, less_than = 10))
        
    def test_numericality_should_return_false_if_value_need_less_than_10_but_it_equals(self):
        self.assertFalse(NumericalityValidator().validate(10, less_than = 10))
        
    def test_numericality_should_return_false_if_value_need_less_than_10_but_it_greater_than(self):
        self.assertFalse(NumericalityValidator().validate(11, less_than = 10))
        
    def test_numericality_less_than_or_equal_to(self):
        self.assertTrue(NumericalityValidator().validate(10, less_than_or_equal_to = 10))
        self.assertTrue(NumericalityValidator().validate(9, less_than_or_equal_to = 10))
    
    def test_numericality_should_return_false_if_value_need_less_than_10_or_equal_but_it_greater_than(self):
        self.assertFalse(NumericalityValidator().validate(11, less_than_or_equal_to = 10))
        
    def test_numericality_odd(self):
        self.assertTrue(NumericalityValidator().validate(11, odd = True))
        
    def test_numericality_not_odd(self):
        self.assertTrue(NumericalityValidator().validate(10, odd = False))
        
    def test_numericality_even(self):
        self.assertTrue(NumericalityValidator().validate(10, even = True))
    
    def test_numericality_not_even(self):
        self.assertTrue(NumericalityValidator().validate(11, even = False))
        

if __name__ == '__main__':
    import unittest
    unittest.main()
