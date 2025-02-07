from typing import Self

from sweet.sequel import Operator
from sweet.sequel.quoting import quote
from sweet.sequel.terms.name import Name
from sweet.utils import is_array


class Pair:
    """
    Represents a SQL condition, which can include comparisons, logical checks, and containment checks.

    A Condition instance is built from a single key-value pair. Special conditions
    are indicated using separators (default: '__') in the key.

    Usage:
        # Normal
        pair = Pair(column1=10)

        # Special condition with operator
        pair = Pair(column1__gte=10)

        # Complex conditions with lists or tuples
        pair = Pair(column1__bt=(5, 15))
    """
    def __init__(self, **kwargs) -> None:
        if len(kwargs) > 1:
            raise ValueError("Only one parameter is allowed for construction.")

        symbol, value = next(iter(kwargs.items()))
        key, op = parse(symbol, value)
        self.field = key
        self.operator = op
        self.value = value

    def __repr__(self) -> str:
        return f'{self.field} {self.operator} {self.value}'

    def __eq__(self, other: Self) -> bool:
        return self.__basic_eq(other) and other.operator == self.operator

    def __hash__(self):
        return hash(f"{self.__class__}-{self.field}-{quote(self.value)}-{self.operator}")

    def __basic_eq(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.field == other.field \
            and (
                self.value == other.value or (
                    isinstance(self.value, (tuple, list)) and isinstance(other.value, (tuple, list)) and list(self.value) == list(other.value)
                )
            )

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
SEPERATOR = '__'


def parse(symbol: str, value: any) -> (str | Name, Operator):
    key, op = symbol, Operator.EQ
    if SEPERATOR in symbol:
        # The symbol represents a general key, such as 'username'
        vs = symbol.split(SEPERATOR)
        op_str = vs[-1]
        if op_str in MAPPING:
            # The symbol represents a special key which included an operator, such as 'username__like'
            op = MAPPING[op_str]
            key = SEPERATOR.join(vs[:-1])
            if (op == Operator.BETWEEN or op == Operator.NOT_BETWEEN) and not (is_array(value) and len(value) == 2):
                raise ValueError(f'The {op_str} operation expects a list or tuple of length 2, but it is not.')

        if SEPERATOR in key:
            # The new_symbol represents a special key which include a parent schema,
            # such as 'users__nickname', 'oa__users__nickname'
            vs = key.split(SEPERATOR)
            vs.reverse()
            key = Name(vs[0], '.'.join(vs[1:]))

    if value is None:
        if op == Operator.EQ: op = Operator.IS
        elif op == Operator.NOT_EQ: op = Operator.IS_NOT
    elif is_array(value):
        if op == Operator.EQ: op = Operator.IN
        elif op == Operator.NOT_EQ: op = Operator.NOT_IN
    return key, op
