from enum import Enum

from sweet.utils import DBDataType


class Operator(Enum):
    IS = "IS"
    IS_NOT = "IS NOT"
    IN = "IN"
    NOT_IN = "NOT IN"
    BETWEEN = "BETWEEN"
    NOT_BETWEEN = "NOT BETWEEN"
    EQ = "="
    NOT_EQ = "<>"
    LT = "<"
    LTE = "<="
    GT = ">"
    GTE = ">="
    LIKE = "LIKE"
    NOT_LIKE = "NOT LIKE"

    def __str__(self) -> str:
        return self.value


class Condition:
    SEPERATOR = '__'

    def __init__(self, **kwargs) -> None:
        if len(kwargs) > 1:
            raise ValueError("Only one parameter is allowed for construction.")
        self.field = None
        self.value = None
        self.operator = Operator.EQ
        for k, v in kwargs.items():
            if self.SEPERATOR not in k:
                self._parse_normal(k, v)
            else:
                self._parse_special(k, v)
            break
        self.field_quoted = f'"{self.field}"'

    def _parse_normal(self, key: str, value: DBDataType) -> None:
        self.field = key
        self.value = value
        if value is None:
            self.operator = Operator.IS
        elif isinstance(value, (tuple, list)):
            self.operator = Operator.IN
        else:
            self.operator = Operator.EQ

    def _parse_special(self, key: str, value: DBDataType) -> None:
        vs = key.split(self.SEPERATOR, 1)
        col, op_type = vs[0], vs[1]
        self.field = col
        self.value = value
        if op_type == 'not':
            if value is None:
                self.operator = Operator.IS_NOT
            elif isinstance(value, (tuple, list)):
                self.operator = Operator.NOT_IN
            else:
                self.operator = Operator.NOT_EQ
        elif op_type == 'bt' or op_type == 'not_bt':
            if not isinstance(value, (tuple, list)) or not len(value) == 2:
                raise ValueError(f'The {op_type} operation expects a list or tuple of length 2, but it is not.')
            self.operator = Operator.BETWEEN if op_type == 'bt' else Operator.NOT_BETWEEN
        elif op_type == 'lt':
            self.operator = Operator.LT
        elif op_type == 'lte':
            self.operator = Operator.LTE
        elif op_type == 'gt':
            self.operator = Operator.GT
        elif op_type == 'gte':
            self.operator = Operator.GTE
        elif op_type == 'like':
            self.operator = Operator.LIKE
        elif op_type == 'not_like':
            self.operator = Operator.NOT_LIKE
        else:
            self._parse_normal(key, value)
