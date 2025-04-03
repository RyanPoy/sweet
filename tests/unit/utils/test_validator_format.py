from sweet.utils.validation import FormatValidator


def test_foramt_with():
    assert FormatValidator().validate('abc.abc.abc.', '[(\w+)/.]{3}') is True


def test_not_format_with():
    assert FormatValidator().validate('aba', '\d+') is False


def test_format_should_return_false_if_value_is_none_and_disallow_null():
    assert FormatValidator().validate(None, None, allow_null=False) is False


def test_format_should_return_true_if_value_is_none_and_allow_null():
    assert FormatValidator().validate(None, None, allow_null=True) is True


def test_format_should_return_false_if_value_is_blank_and_disallow_blank():
    assert FormatValidator().validate('     ', **{'_with': '\s+', 'allow_blank': False}) is False
    assert FormatValidator().validate('     ', **{'_with': '\s+', 'allow_blank': True}) is True
