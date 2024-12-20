from sweet.sequel.terms import Term


class Name(Term):

    def __init__(self, name: str):
        self.value = name

    def __repr__(self):
        return f'{self.__class__.__name__}("{self.value}")'


class ColumnName(Name): pass
class TableName(Name): pass
class AliasName(Name): pass
