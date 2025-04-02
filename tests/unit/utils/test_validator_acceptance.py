from sweet.utils.validation import AcceptanceValidator


def test_acceptance():
    assert AcceptanceValidator().validate(10) is True


def test_acceptance_should_return_false_if_disallow_null():
    assert AcceptanceValidator().validate(None, allow_null=False) is False


def test_acceptance_should_return_true_if_allow_null():
    assert AcceptanceValidator().validate(None) is True
