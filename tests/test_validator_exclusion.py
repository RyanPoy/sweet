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

from pyrails.activerecord import ExclusionValidator
import unittest


class ExclusionValidatorTest(unittest.TestCase):

    def test_exclusion_if_value_is_null_and_allow_null(self):
        self.assertTrue(ExclusionValidator().validate(None, allow_null = True))
        
    def test_exclusion_if_value_is_null_and_disallow_null(self):
        self.assertFalse(ExclusionValidator().validate(None))
    
    def test_exclusion_if_value_is_blank_str_and_allow_blank(self):
        self.assertTrue(ExclusionValidator().validate('   ', allow_blank = True))

    def test_exclusion_if_value_is_blank_str_and_disallow_blank(self):
        self.assertFalse(ExclusionValidator().validate('   '))

    def test_exclusion(self):
        self.assertFalse(ExclusionValidator().validate('poy', ['poy', 'pengyi']))
        
    def test_exclusion_if_value_not_in(self):
        self.assertTrue(ExclusionValidator().validate('poy', ['peng', 'ryan']))
        

if __name__ == '__main__':
    unittest.main()
