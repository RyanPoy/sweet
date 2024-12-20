from sweet.sequel.terms import Term


class Name(Term):

    def __init__(self, s: str):
        self.name = s


class ColumnName(Name): pass
class TableName(Name): pass
class AliasName(Name): pass
