from sweet.utils.validation import NumericalityValidator


def test_numericality_should_return_false_if_value_is_null():
    assert NumericalityValidator().validate(None, greater_than=10) is False


def test_numericality_should_return_false_if_value_is_not_a_number():
    assert NumericalityValidator().validate('s', greater_than=10) is False


def test_numericality_greate_than():
    assert NumericalityValidator().validate(11, greater_than=10) is True


def test_numericality_should_return_false_if_value_need_greater_than_10_but_it_equals():
    assert NumericalityValidator().validate(10, greater_than=10) is False


def test_numericality_should_return_false_if_value_need_greater_than_10_but_it_less_than():
    assert NumericalityValidator().validate(9, greater_than=10) is False


def test_numericality_great_than_or_equal_to():
    assert NumericalityValidator().validate(10, greater_than_or_equal_to=10) is True
    assert NumericalityValidator().validate(11, greater_than_or_equal_to=10) is True


def test_numericality_should_return_false_if_value_need_greater_than_10_or_equal_but_it_less_than():
    assert NumericalityValidator().validate(9, greater_than_or_equal_to=10) is False


def test_numericality_equal_to():
    assert NumericalityValidator().validate(10, equal_to=10) is True


def test_numericality_should_return_false_if_value_need_equal_10_but_it_less_than():
    assert NumericalityValidator().validate(9, equal_to=10) is False


def test_numericality_should_return_false_if_value_need_equal_10_but_it_greate_than():
    assert NumericalityValidator().validate(11, equal_to=10) is False


def test_numericality_less_than():
    assert NumericalityValidator().validate(9, less_than=10) is True


def test_numericality_should_return_false_if_value_need_less_than_10_but_it_equals():
    assert NumericalityValidator().validate(10, less_than=10) is False


def test_numericality_should_return_false_if_value_need_less_than_10_but_it_greater_than():
    assert NumericalityValidator().validate(11, less_than=10) is False


def test_numericality_less_than_or_equal_to():
    assert NumericalityValidator().validate(10, less_than_or_equal_to=10) is True
    assert NumericalityValidator().validate(9, less_than_or_equal_to=10) is True


def test_numericality_should_return_false_if_value_need_less_than_10_or_equal_but_it_greater_than():
    assert NumericalityValidator().validate(11, less_than_or_equal_to=10) is False


def test_numericality_odd():
    assert NumericalityValidator().validate(11, odd=True) is True


def test_numericality_not_odd():
    assert NumericalityValidator().validate(10, odd=False) is True


def test_numericality_even():
    assert NumericalityValidator().validate(10, even=True) is True


def test_numericality_not_even():
    assert NumericalityValidator().validate(11, even=False) is True
