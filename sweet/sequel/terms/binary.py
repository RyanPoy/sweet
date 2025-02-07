from typing import Self

from sweet.sequel import Operator
from sweet.sequel.terms.value import Value1
from sweet.utils import is_array
from sweet.sequel.terms.name import Name


class Binary:

    def __init__(self, op: Operator, key, value: Value1 = None) -> None:
        self.key: Name | str = key
        self.value: Value1 = value
        self.op: Operator = op

    def invert(self) -> Self:
        self.op = self.op.invert()
        return self

    def __eq__(self, other: Self) -> bool:
        if self.__class__ != other.__class__ or self.key != other.key or self.op != other.op:
            return False
        if is_array(self.value) and is_array(other.value):
            return list(self.value) == list(other.value)
        if not is_array(self.value) and not is_array(other.value):
            return self.value == other.value
        return False

    def __hash__(self) -> str:
        new_value = self.value
        if is_array(self.value):
            new_value = ''.join([str(v) for v in self.value])
        return f'{self.__class__}-{hash(self.key)}-{hash(new_value)}-{self.op}'


MAPPING = {
    ''         : Operator.EQ,
    'not'      : Operator.EQ.invert(),

    'bt'       : Operator.BETWEEN,
    'not_bt'   : Operator.BETWEEN.invert(),

    'lt'       : Operator.LT,
    'not_lt'   : Operator.LT.invert(),

    'lte'      : Operator.LTE,
    'not_lte'  : Operator.LTE.invert(),

    'gt'       : Operator.GT,
    'not_gt'   : Operator.GT.invert(),

    'gte'      : Operator.GTE,
    'not_gte'  : Operator.GTE.invert(),

    'like'     : Operator.LIKE,
    'not_like' : Operator.LIKE.invert(),

    'regex'    : Operator.REGEX,
    'not_regex': Operator.REGEX.invert(),
}


def parse(**kwargs: {str: Value1}) -> Binary:
    if len(kwargs) != 1:
        raise ValueError('Only one parameter is allowed for construction.')

    symbol, value = next(iter(kwargs.items()))  # symbol: str, value: Value
    key, op, seperator = symbol, Operator.EQ, '__'
    if seperator in symbol:
        # The symbol represents a general key, such as 'username'
        parts = symbol.split(seperator)
        op_str = parts[-1]
        if op_str in MAPPING:
            # The symbol represents a special key which included an operator, such as 'username__like'
            op = MAPPING[op_str]
            key = seperator.join(parts[:-1])
            if op in {Operator.BETWEEN, Operator.NOT_BETWEEN} and not (is_array(value) and len(value) == 2):
                raise ValueError(f'The {op_str} operation expects a list or tuple of length 2, but it is not.')

        if seperator in key:
            # The new_symbol represents a special key which include a parent schema,
            # such as 'users__nickname', 'oa__users__nickname'
            reversed_parts = key.split(seperator)[::-1]
            key = Name(reversed_parts[0], '.'.join(reversed_parts[1:]))

    if value is None:
        if op == Operator.EQ:
            op = Operator.IS
        elif op == Operator.NOT_EQ:
            op = Operator.IS_NOT
    elif is_array(value):
        if op == Operator.EQ:
            op = Operator.IN
        elif op == Operator.NOT_EQ:
            op = Operator.NOT_IN
    return Binary(op, key, value)
