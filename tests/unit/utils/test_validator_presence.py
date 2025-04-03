from sweet.utils.validation import PresenceValidator


def test_presence_validate_if_value_is_none():
    assert PresenceValidator().validate(None) is False


def test_presence_validate_if_value_is_not_empty_str():
    assert PresenceValidator().validate('pengyi') is True
    assert PresenceValidator().validate('pengyi', allow_blank=True) is True


def test_presence_validate_should_return_true_is_value_is_empty_str_and_allow_blank():
    assert PresenceValidator().validate('') is True


def test_presence_validate_should_return_false_is_value_is_empty_str_and_disallow_blank():
    assert PresenceValidator().validate('', allow_blank=False) is False


def test_presence_validate_should_return_false_if_value_is_blank_str_and_disallow_blank():
    assert PresenceValidator().validate('  ', allow_blank=False) is False


def test_presence_validate_should_return_true_if_value_is_empty_list_and_allow_blank():
    assert PresenceValidator().validate([]) is True


def test_presence_validate_should_return_true_if_value_is_empty_list_and_disallow_blank():
    assert PresenceValidator().validate([], allow_blank=False) is False


def test_presence_validate_should_return_true_if_value_is_list_has_any_element():
    assert PresenceValidator().validate(['a']) is True
    assert PresenceValidator().validate(['a'], allow_blank=True) is True


def test_presence_validate_should_return_true_if_value_is_empty_hash_and_disallow_blank():
    assert PresenceValidator().validate({}, allow_blank=False) is False


def test_presence_validate_should_return_true_if_value_is_empty_hash_and_allow_blank():
    assert PresenceValidator().validate({}) is True


def test_presence_validate_should_return_true_if_value_is_hash_has_any_element():
    assert PresenceValidator().validate({'a': 'b'}) is True
    assert PresenceValidator().validate({'a': 'b'}, allow_blank=True) is True
