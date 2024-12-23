from sweet.sequel.terms import Term
from sweet.utils import DBDataType


class Value(Term):

    def __init__(self, v: DBDataType):
        self.v = v

