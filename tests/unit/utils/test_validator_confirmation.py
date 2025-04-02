from sweet.utils.validation import ConfirmationValidator


def test_confirmation():
    assert ConfirmationValidator().validate(10, 10) is True


def test_disconfirmation():
    assert ConfirmationValidator().validate(10, 100) is False
