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

from pyrails.activerecord import LengthValidator
import unittest


class LengthValidatorTest(unittest.TestCase):

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
        self.assertTrue(LengthValidator().validate(None, allow_null = True))
        
    def test_length_should_return_false_if_value_is_null_and_disallow_null(self):
        self.assertFalse(LengthValidator().validate(None, allow_null = False))

    def test_length_should_return_true_if_value_is_blank_str_and_allow_blank(self):
        self.assertTrue(LengthValidator().validate('     ', allow_blank = True))
        
    def test_length_should_return_false_if_value_is_blank_and_disallow_blank(self):
        self.assertFalse(LengthValidator().validate([], allow_blank = False))
        

if __name__ == '__main__':
    unittest.main()
