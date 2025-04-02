import unittest
from sweet.utils.validation import LengthValidator


def test_length_is_5():
    assert LengthValidator().validate('hello', _is=5) is True


def test_length_is_not_5():
    assert LengthValidator().validate('hello, world', _is=5) is False


def test_length_minimum_is_5():
    assert LengthValidator().validate('hello, world', minimum=5) is True
    assert LengthValidator().validate('hello', minimum=5) is True


def test_length_should_return_false_if_length_less_than_minimum():
    assert LengthValidator().validate('hi', minimum=5) is False


def test_length_maximun_is_10():
    assert LengthValidator().validate('hello', maximum=10) is True
    assert LengthValidator().validate('diplayport', maximum=10) is True


def test_length_should_return_false_if_length_greater_than_maximum():
    assert LengthValidator().validate('validations.value', maximum=10) is False


def test_length_should_return_true_if_value_is_null_and_allow_null():
    assert LengthValidator().validate(None, allow_null=True) is True


def test_length_should_return_false_if_value_is_null_and_disallow_null():
    assert LengthValidator().validate(None, allow_null=False) is False


def test_length_should_return_true_if_value_is_blank_str_and_allow_blank():
    assert LengthValidator().validate('     ', allow_blank=True) is True


def test_length_should_return_false_if_value_is_blank_and_disallow_blank():
    assert LengthValidator().validate([], allow_blank=False) is False


if __name__ == '__main__':
    unittest.main()
