import copy
from typing import Self

from sweet.sequel.terms import Logic, literal
from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.name import Name
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class Fn:

    def __init__(self, name: str, parentheses: bool = True) -> None:
        self.name = name
        self.parentheses = parentheses
        self.alias = ""
        self.columns = []
        self.cmp_pairs = []
        self.chain = []
        self.nesting = None

    def column(self, *column_names: Name | Literal | Self) -> Self:
        if column_names == [literal.STAR]:
            return self
        cns = []
        for c in column_names:
            if isinstance(c, (Name, Literal)):
                cns.append(c)
            elif isinstance(c, Fn):
                if c.alias:
                    new_fn = copy.deepcopy(c)
                    new_fn.alias = ""
                    self.nesting = new_fn
                else:
                    self.nesting = c
            else:
                raise ValueError(f"Support Name and Literal，but got[{c}] ")

        if len(self.columns) == 1 and self.columns[0] == literal.STAR:
            self.columns = cns
        else:
            self.columns.extend(cns)
        return self

    def distinct(self) -> Self:
        dis = distinct()
        self_copied = copy.deepcopy(self)
        self_copied.name = dis.name # change self_copied to distinct Fn
        self_copied.parentheses = False
        new_self = Fn(self.name)

        new_self.nesting = self_copied
        return new_self

    def as_(self, name: str) -> Self:
        self.alias = Name(name)
        return self

    def __gt__(self, other: Name | DBDataType) -> Self:
        return self.__compare(">", other)

    def __ge__(self, other: Name | DBDataType) -> Self:
        return self.__compare(">=", other)

    def __lt__(self, other: Name | DBDataType) -> Self:
        return self.__compare("<", other)

    def __le__(self, other: Name | DBDataType) -> Self:
        return self.__compare("<=", other)

    def __eq__(self, other: Name | DBDataType) -> Self:
        return self.__compare("=", other)

    def __ne__(self, other: Name | DBDataType) -> Self:
        return self.__compare("<>", other)

    def __and__(self, other: Self) -> Self:
        return self.__combine(other, Logic.AND)

    def __or__(self, other: Self) -> Self:
        return self.__combine(other, Logic.OR)

    def __combine(self, other: Self, logic_op: Logic) -> Self:
        """Combines two Q objects with a logical operator (AND, OR)."""
        if not isinstance(other, Fn):
            raise TypeError("Logical operators can only be applied between two Fn objects.")

        self.chain.append((logic_op, other))
        return self

    def __compare(self, op: str, value: Name | DBDataType) -> Self:
        if isinstance(value, Name):
            self.cmp_pairs.append((op, value))
        else:
            self.cmp_pairs.append((op, Value(value)))
        return self


count = lambda *columns: Fn("COUNT").column(*columns)
sum = lambda *columns: Fn("SUM").column(*columns)
avg = lambda *columns: Fn("AVERAGE").column(*columns)
sqrt = lambda *columns: Fn("SQRT").column(*columns)
distinct = lambda *columns: Fn("DISTINCT", False).column(*columns)
