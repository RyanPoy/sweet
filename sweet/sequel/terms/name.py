from __future__ import annotations

import copy
from dataclasses import dataclass
from typing import Self, Sequence, TYPE_CHECKING, Union
from sweet.sequel.types import V

if TYPE_CHECKING:
    from sweet.sequel.terms.binary import Binary

from sweet.sequel import Operator
from sweet.sequel.terms.literal import Literal, STAR
from sweet.utils import is_array


@dataclass
class Name:
    name: str
    schema_name: str = None
    alias: str = None

    def __post_init__(self) -> None:
        if isinstance(self.schema_name, Name):
            self.schema_name = self.schema_name.alias if self.schema_name.alias else self.schema_name.name

    def as_(self, alias: str) -> Self:
        self.alias = alias
        return self

    def rm_alias(self) -> Self:
        instance = copy.copy(self)
        instance.alias = None
        return instance

    def __eq__(self, other: Union[Self | Literal | str]) -> bool:
        if isinstance(other, str) and other == '*' and self.name == '*':
            return True
        if isinstance(other, Literal) and other == STAR and self.name == '*':
            return True
        if self.__class__ != other.__class__:
            return False
        return self.name == other.name and self.schema_name == other.schema_name and self.alias == other.alias

    def eq(self, v: V) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS, v)
        elif is_array(v):
            return self._binary(Operator.IN, v)
        else:
            return self._binary(Operator.EQ, v)

    def not_eq(self, v: V) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS_NOT, v)
        elif is_array(v):
            return self._binary(Operator.NOT_IN, v)
        else:
            return self._binary(Operator.NOT_EQ, v)

    def gt(self, v: V) -> 'Binary':
        return self._binary(Operator.GT, v)

    def not_gt(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_GT, v)

    def gte(self, v: V) -> 'Binary':
        return self._binary(Operator.GTE, v)

    def not_gte(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_GTE, v)

    def lt(self, v: V) -> 'Binary':
        return self._binary(Operator.LT, v)

    def not_lt(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LT, v)

    def lte(self, v: V) -> 'Binary':
        return self._binary(Operator.LTE, v)

    def not_lte(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LTE, v)

    def like(self, v: V) -> 'Binary':
        return self._binary(Operator.LIKE, v)

    def not_like(self, v: V) -> 'Binary':
        return self._binary(Operator.NOT_LIKE, v)

    def between(self, v: Sequence[V]) -> 'Binary':
        return self._binary(Operator.BETWEEN, v)

    def not_between(self, v: Sequence[V]) -> 'Binary':
        return self._binary(Operator.NOT_BETWEEN, v)

    def regex(self, v: str) -> 'Binary':
        return self._binary(Operator.REGEX, v)

    def not_regex(self, v: str) -> 'Binary':
        return self._binary(Operator.NOT_REGEX, v)

    def _binary(self, op, v) -> 'Binary':
        from sweet.sequel.terms.binary import Binary
        return Binary(self, op, v)
