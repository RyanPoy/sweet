from enum import Enum
from sweet.sequel.terms import Term, literal
from sweet.sequel.terms.name import ColumnName
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class SortedIn(Enum):
    ASC = 'ASC'
    DESC = 'DESC'

    def __str__(self):
        return self.value


class OrderClause(Term):

    def __init__(self, *column_names: ColumnName | DBDataType, sorted_in: SortedIn = None) -> None:
        self.orders = []
        self.sorted_in: SortedIn = sorted_in

        for c in column_names:
            if c == '*':
                self.orders.append(literal.STAR)
            if isinstance(c, ColumnName):
                if c.alias:
                    self.orders.append(ColumnName(c.alias))
                else:
                    self.orders.append(c)
            elif isinstance(c, (str, )):
                self.orders.append(ColumnName(c))
            else:
                self.orders.append(Value(c))
