from __future__ import annotations
import copy
from dataclasses import dataclass
from typing import List, Self, Sequence, TYPE_CHECKING

from sweet.sequel.terms.literal import Literal, STAR
from sweet.sequel.types import RawType

if TYPE_CHECKING:
    from sweet.sequel.terms.binary import Binary

from sweet.utils import is_array
from sweet.sequel import Operator


@dataclass
class ExtType:
    name: str
    alias: str = None

    def as_(self, name: str) -> Self:
        self.alias = name
        return self

    def rm_alias(self) -> Self:
        instance = copy.copy(self)
        instance.alias = None
        return instance

    def eq(self, v) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS, v)
        elif is_array(v):
            return self._binary(Operator.IN, v)
        else:
            return self._binary(Operator.EQ, v)

    def not_eq(self, v) -> 'Binary':
        if v is None:
            return self._binary(Operator.IS_NOT, v)
        elif is_array(v):
            return self._binary(Operator.NOT_IN, v)
        else:
            return self._binary(Operator.NOT_EQ, v)

    def gt(self, v) -> 'Binary':
        return self._binary(Operator.GT, v)

    def not_gt(self, v) -> 'Binary':
        return self._binary(Operator.NOT_GT, v)

    def gte(self, v) -> 'Binary':
        return self._binary(Operator.GTE, v)

    def not_gte(self, v) -> 'Binary':
        return self._binary(Operator.NOT_GTE, v)

    def lt(self, v) -> 'Binary':
        return self._binary(Operator.LT, v)

    def not_lt(self, v) -> 'Binary':
        return self._binary(Operator.NOT_LT, v)

    def lte(self, v) -> 'Binary':
        return self._binary(Operator.LTE, v)

    def not_lte(self, v) -> 'Binary':
        return self._binary(Operator.NOT_LTE, v)

    def like(self, v) -> 'Binary':
        return self._binary(Operator.LIKE, v)

    def not_like(self, v) -> 'Binary':
        return self._binary(Operator.NOT_LIKE, v)

    def between(self, v: Sequence) -> 'Binary':
        return self._binary(Operator.BETWEEN, v)

    def not_between(self, v: Sequence) -> 'Binary':
        return self._binary(Operator.NOT_BETWEEN, v)

    def regex(self, v: str) -> 'Binary':
        return self._binary(Operator.REGEX, v)

    def not_regex(self, v: str) -> 'Binary':
        return self._binary(Operator.NOT_REGEX, v)

    def _binary(self, op, v) -> 'Binary':
        from sweet.sequel.terms.binary import Binary
        return Binary(self, op=op, value=v)


###############################
#################################
######################################
##########################################
@dataclass
class Name(ExtType):
    schema_name: str = None

    def __init__(self, name: str, schema_name: str | Name = None):
        super().__init__(name, None)
        if schema_name is None or isinstance(schema_name, str):
            self.schema_name = schema_name
        elif isinstance(schema_name, Name):
            self.schema_name = schema_name.alias if schema_name.alias else schema_name.name
        else:
            raise TypeError(f"Name initialize schema_name with str or Name, but got a {schema_name.__class__.__name__}")

    def __eq__(self, other: Self | Literal | str) -> bool:
        if isinstance(other, str) and other == '*' and self.name == '*':
            return True
        if isinstance(other, Literal) and other == STAR and self.name == '*':
            return True
        if self.__class__ != other.__class__:
            return False
        return self.name == other.name and self.schema_name == other.schema_name and self.alias == other.alias

    def __hash__(self):
        return hash(f'{self.schema_name}-{self.name}-{self.alias}')

    def copy(self) -> Self:
        return copy.deepcopy(self)


###############################
#################################
######################################
##########################################


@dataclass
class Fn(ExtType):
    columns: List[RawType | ExtType | Literal] = None
    _distinct: bool = False

    def __post_init__(self) -> None:
        if self.columns is None:
            self.columns = []

    def column(self, *column_names: RawType | ExtType | Literal) -> Self:
        for c in column_names:
            if c == '*' or c == STAR:
                self.columns.append(STAR)
            elif isinstance(c, Literal):
                self.columns.append(c)
            elif isinstance(c, (Name, Fn)):
                self.columns.append(c.rm_alias())
            elif isinstance(c, str):
                self.columns.append(Name(c))
            else:
                raise ValueError(f"Support Name and Literal，but got[{c.__class__}] ")
        return self

    def is_distinct(self) -> bool:
        return self._distinct

    def distinct(self) -> Self:
        self._distinct = True
        return self


Count = lambda *columns: Fn("COUNT").column(*columns)
Sum = lambda *columns: Fn("SUM").column(*columns)
Avg = lambda *columns: Fn("AVERAGE").column(*columns)
Sqrt = lambda *columns: Fn("SQRT").column(*columns)
