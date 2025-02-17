import copy
from dataclasses import dataclass
from datetime import date, datetime
from decimal import Decimal
from typing import Self, Union

from sweet.sequel import Operator
from sweet.sequel.logic import Logic
from sweet.sequel.terms.name_fn import Name, Fn
from sweet.sequel.types import Array, ArrayType, Raw, RawType
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
    key: Union[Raw, Name, Fn]
    op: Operator
    value: Union[Raw, Name, Fn, Array]

    logic: Logic = None
    left: 'Binary' = None
    right: 'Binary' = None
    inverted: bool = False
    parent: 'Binary' = None

    def __init__(self, key: Union[RawType, Name, Fn], op: Operator, value: Union[RawType, Name, Fn, ArrayType]) -> None:
        if isinstance(key, RawType):
            self.key = Raw(key)
        elif isinstance(key, (Name, Fn)):
            self.key = key
        else:
            raise TypeError(f"key must be a RawType，Name, Fn. But got {key.__class__}")

        self.op = op

        if isinstance(value, RawType):
            self.value = Raw(value)
        elif isinstance(value, (Name, Fn)):
            self.value = value
        elif isinstance(value, (list, tuple)):
            self.value = Array(value)
        else:
            raise TypeError(f"key must be a RawType，Name, Fn, List, Tuple. But got {key.__class__}")

        if (self.op in {Operator.BETWEEN, Operator.NOT_BETWEEN} and
                not (isinstance(self.value, Array) and self.value.valid_for_between())):
            raise ValueError(f'The "{self.op}" operation expects a list or tuple of length 2, but it is not.')

    def for_logic(self) -> bool:
        return self.op.is_empty()

    def is_empty(self) -> bool:
        return (self.op.is_empty() and self.key is None and self.value is None) or (self.op.is_empty() and self.key == Raw(None) and self.value == Raw(None)) and self.left is None

    def __eq__(self, other) -> bool:
        if super().__eq__(other):
            return True
        if self.is_empty() and other.is_empty():
            return True
        return False

    def __invert__(self) -> Self:
        b = copy.deepcopy(self)
        b.inverted = not b.inverted
        return b

    def __and__(self, other: Self) -> Self:
        return self.__logic(Logic.AND, other)

    def __or__(self, other: Self) -> Self:
        return self.__logic(Logic.OR, other)

    def __logic(self, logic: Logic, other: Self) -> Self:
        if other.__class__ != self.__class__:
            raise TypeError(f"Binary {logic} operate with an other Binary instance, but got a {other.__class__.__name__} instance.")
        if self.is_empty():
            return other
        if other.is_empty():
            return self

        b = self.__class__.empty()
        b.logic = logic
        b.left = self
        b.right = other
        self.parent = b
        other.parent = b
        return b

    @classmethod
    def empty(cls):
        return cls(None, Operator.Empty, None)

    @classmethod
    def parse(cls, **kwargs: Union[RawType, Name, Fn, ArrayType]) -> Self:
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
                key = Name(reversed_parts[0], schema_name='.'.join(reversed_parts[1:]))

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

        if isinstance(key, str):
            key = Name(key)
        return cls(key, op, value)

    def is_a_special_operation(self) -> bool:
        return self.op in {Operator.IS, Operator.IS_NOT, Operator.IN, Operator.NOT_IN, Operator.LIKE, Operator.NOT_LIKE, Operator.REGEX, Operator.NOT_REGEX}

    def is_a_between_operation(self) -> bool:
        return self.op in {Operator.BETWEEN, Operator.NOT_BETWEEN}


def Q(**kwargs: Union[RawType, Name, Fn, ArrayType]) -> Binary:
    b = None
    for k, v in kwargs.items():
        if b is None:
            b = Binary.parse(**{k: v})
        else:
            b = b & Binary.parse(**{k: v})
    if b is not None:
        return b
    return Binary.empty()
