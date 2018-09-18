#coding: utf8
from sweet.utils.validation import ExclusionValidator
import unittest


class ExclusionValidatorTest(unittest.TestCase):

    def test_exclusion_if_value_is_null_and_allow_null(self):
        self.assertTrue(ExclusionValidator().validate(None, allow_null=True))
        
    def test_exclusion_if_value_is_null_and_disallow_null(self):
        self.assertFalse(ExclusionValidator().validate(None))
    
    def test_exclusion_if_value_is_blank_str_and_allow_blank(self):
        self.assertTrue(ExclusionValidator().validate('   ', allow_blank=True))

    def test_exclusion_if_value_is_blank_str_and_disallow_blank(self):
        self.assertFalse(ExclusionValidator().validate('   '))

    def test_exclusion(self):
        self.assertFalse(ExclusionValidator().validate('poy', ['poy', 'pengyi']))
        
    def test_exclusion_if_value_not_in(self):
        self.assertTrue(ExclusionValidator().validate('poy', ['peng', 'ryan']))
        

if __name__ == '__main__':
    unittest.main()
