#coding: utf8
from sweet.utils.validation import PresenceValidator
from sweet._tests import TestCase


class TestValidatorPresence(TestCase):

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
    import unittest
    unittest.main()
