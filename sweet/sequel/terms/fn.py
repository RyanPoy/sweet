from typing import Self

from sweet.sequel.terms import Term, literal
from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.name import ColumnName
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class Fn(Term):

    def __init__(self, name: str) -> None:
        super().__init__()
        self.name = name
        self.is_distinct = False
        self.columns = [literal.STAR]
        self.cmp_pairs = []

    def column(self, *column_names: [ColumnName | Literal | str]) -> Self:
        if column_names == ["*"] or column_names == [literal.STAR]:
            return self
        cns = []
        for c in column_names:
            if c == '*':
                cns.append(literal.STAR)
            elif isinstance(c, (str,)):
                cns.append(ColumnName(c))
            else:
                cns.append(c)
        if len(self.columns) == 1 and self.columns[0] == literal.STAR:
            self.columns = cns
        else:
            self.columns.extend(cns)
        return self

    def distinct(self) -> Self:
        self.is_distinct = True
        return self

    def __gt__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare(">", other)

    def __ge__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare(">=", other)

    def __lt__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare("<", other)

    def __le__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare("<=", other)

    def __eq__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare("=", other)

    def __ne__(self, other: ColumnName | DBDataType) -> Self:
        return self.__compare("<>", other)

    def __compare(self, op: str, value: ColumnName | DBDataType) -> Self:
        if isinstance(value, ColumnName):
            self.cmp_pairs.append((op, value))
        else:
            self.cmp_pairs.append((op, Value(value)))
        return self


count = lambda *columns: Fn("COUNT").column(*columns)
sum = lambda *columns: Fn("SUM").column(*columns)
avg = lambda *columns: Fn("AVERAGE").column(*columns)
