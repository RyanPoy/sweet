from dataclasses import dataclass
from typing import Self

from sweet.sequel import Operator
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.types import K, V
from sweet.utils import is_array

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


@dataclass
class Binary:
    key: K
    op: Operator
    value: V

    def __post_init__(self) -> None:
        if isinstance(self.value, list):
            self.value = tuple(self.value)
        if isinstance(self.key, str):
            self.key = Name(self.key)
        if self.op in {Operator.BETWEEN, Operator.NOT_BETWEEN} and not (is_array(self.value) and len(self.value) == 2):
            raise ValueError(f'The "{self.op}" operation expects a list or tuple of length 2, but it is not.')

    @classmethod
    def parse(cls, **kwargs: V) -> Self:
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
        return cls(key, op, value)
