#coding: utf8
import unittest
from sweet.utils.validation import InclusionValidator


class TestValidatorInclusion(unittest.TestCase):

    def test_inclusion_if_value_is_null_and_allow_null(self):
        self.assertTrue(InclusionValidator().validate(None, allow_null=True))
        
    def test_inclusion_if_value_is_null_and_disallow_null(self):
        self.assertFalse(InclusionValidator().validate(None))
    
    def test_inclusion_if_value_is_blank_str_and_allow_blank(self):
        self.assertTrue(InclusionValidator().validate('   ', allow_blank=True))

    def test_inclusion_if_value_is_blank_str_and_disallow_blank(self):
        self.assertFalse(InclusionValidator().validate('   '))

    def test_inclusion(self):
        self.assertTrue(InclusionValidator().validate('poy', ['poy']))
        
    def test_inclusion_if_value_not_in(self):
        self.assertFalse(InclusionValidator().validate('poy', ['peng', 'ryan']))


if __name__ == '__main__':
    unittest.main()
