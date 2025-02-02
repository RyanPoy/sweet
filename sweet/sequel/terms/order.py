from enum import Enum
from sweet.sequel.terms import literal
from sweet.sequel.terms.name import Name
from sweet.sequel.terms.value import Value
from sweet.utils import DBDataType


class SortedIn(Enum):
    ASC = 'ASC'
    DESC = 'DESC'

    def __str__(self):
        return self.value


class OrderClause:

    def __init__(self, *column_names: Name, sorted_in: SortedIn = None) -> None:
        self.orders = []
        self.sorted_in: SortedIn = sorted_in
        for c in column_names:
            if c.alias:
                self.orders.append(Name(c.alias))
            else:
                self.orders.append(c)
