from sweet.utils.validation import ExclusionValidator


def test_exclusion_if_value_is_null_and_allow_null():
    assert ExclusionValidator().validate(None, allow_null=True) is True


def test_exclusion_if_value_is_null_and_disallow_null():
    assert ExclusionValidator().validate(None) is False


def test_exclusion_if_value_is_blank_str_and_allow_blank():
    assert ExclusionValidator().validate('   ', allow_blank=True) is True


def test_exclusion_if_value_is_blank_str_and_disallow_blank():
    assert ExclusionValidator().validate('   ') is False


def test_exclusion():
    assert ExclusionValidator().validate('poy', ['poy', 'pengyi']) is False


def test_exclusion_if_value_not_in():
    assert ExclusionValidator().validate('poy', ['peng', 'ryan']) is True
