#coding: utf8
from sweet.utils.validation import FormatValidator
from sweet._tests import TestCase


class TestValidatorFormat(TestCase):

    def test_foramt_with(self):
        self.assertTrue(FormatValidator().validate('abc.abc.abc.', '[(\w+)/.]{3}'))  

    def test_not_format_with(self):
        self.assertFalse(FormatValidator().validate('aba', '\d+'))
    
    def test_format_should_return_false_if_value_is_none_and_disallow_null(self):
        self.assertFalse(FormatValidator().validate(None, None, allow_null=False))
        
    def test_format_should_return_true_if_value_is_none_and_allow_null(self):
        self.assertTrue(FormatValidator().validate(None, None, allow_null=True))

    def test_format_should_return_false_if_value_is_blank_and_disallow_blank(self):
        self.assertFalse(FormatValidator().validate('     ', **{'_with': '\s+', 'allow_blank': False}))
        self.assertTrue(FormatValidator().validate('     ', **{'_with': '\s+', 'allow_blank': True}))


if __name__ == '__main__':
    import unittest
    unittest.main()
