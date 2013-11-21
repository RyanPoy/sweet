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

from pyrails.activerecord import PresenceValidator
import unittest


class PresenceValidatorTest(unittest.TestCase):

    def test_presence_validate_if_value_is_none(self):
        self.assertFalse(PresenceValidator().validate(None))
        
    def test_presence_validate_if_value_is_not_empty_str(self):
        self.assertTrue(PresenceValidator().validate('pengyi'))
        self.assertTrue(PresenceValidator().validate('pengyi', allow_blank=True))
        
    def test_presence_validate_should_return_true_is_value_is_empty_str_and_allow_blank(self):
        self.assertTrue(PresenceValidator().validate(''))
        
    def test_presence_validate_should_return_false_is_value_is_empty_str_and_disallow_blank(self):
        self.assertFalse(PresenceValidator().validate('', allow_blank=False))
        
    def test_presence_validate_should_return_false_if_value_is_blank_str_and_disallow_blank(self):
        self.assertFalse(PresenceValidator().validate('  ', allow_blank=False))
        
    def test_presence_validate_should_return_true_if_value_is_empty_list_and_allow_blank(self):
        self.assertTrue(PresenceValidator().validate([]))
    
    def test_presence_validate_should_return_true_if_value_is_empty_list_and_disallow_blank(self):
        self.assertFalse(PresenceValidator().validate([], allow_blank=False))
        
    def test_presence_validate_should_return_true_if_value_is_list_has_any_element(self):
        self.assertTrue(PresenceValidator().validate(['a']))
        self.assertTrue(PresenceValidator().validate(['a'], allow_blank=True))
    
    def test_presence_validate_should_return_true_if_value_is_empty_hash_and_disallow_blank(self):
        self.assertFalse(PresenceValidator().validate({}, allow_blank=False))
        
    def test_presence_validate_should_return_true_if_value_is_empty_hash_and_allow_blank(self):
        self.assertTrue(PresenceValidator().validate({}))
        
    def test_presence_validate_should_return_true_if_value_is_hash_has_any_element(self):
        self.assertTrue(PresenceValidator().validate({'a': 'b'}))
        self.assertTrue(PresenceValidator().validate({'a': 'b'}, allow_blank=True))
        

if __name__ == '__main__':
    unittest.main()
