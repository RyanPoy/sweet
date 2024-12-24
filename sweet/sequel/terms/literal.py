from sweet.sequel.terms import Term


class Literal(Term):

    def __init__(self, v):
        super().__init__()
        self.v = v


STAR = Literal("*")
