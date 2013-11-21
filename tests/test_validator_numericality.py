#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

from pyrails.activerecord import NumericalityValidator, NoOptionError, OptionNameError
import unittest


class NumericalityValidatorTest(unittest.TestCase):

    def test_numericality_should_get_error_if_no_options(self):
        self.assertRaises(NoOptionError, NumericalityValidator().validate, 10)
        
    def test_numericality_should_get_error_if_exclusion_options(self):
        self.assertRaises(OptionNameError, NumericalityValidator().validate, 10, **{'fuck': 'fuck'})
        
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
    unittest.main()
