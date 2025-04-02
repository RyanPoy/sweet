import unittest
from sweet.utils.validation import InclusionValidator


def test_inclusion_if_value_is_null_and_allow_null():
    assert InclusionValidator().validate(None, allow_null=True) is True


def test_inclusion_if_value_is_null_and_disallow_null():
    assert InclusionValidator().validate(None) is False


def test_inclusion_if_value_is_blank_str_and_allow_blank():
    assert InclusionValidator().validate('   ', allow_blank=True) is True


def test_inclusion_if_value_is_blank_str_and_disallow_blank():
    assert InclusionValidator().validate('   ') is False


def test_inclusion():
    assert InclusionValidator().validate('poy', ['poy']) is True


def test_inclusion_if_value_not_in():
    assert InclusionValidator().validate('poy', ['peng', 'ryan']) is False


if __name__ == '__main__':
    unittest.main()
