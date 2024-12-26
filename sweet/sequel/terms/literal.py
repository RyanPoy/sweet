from sweet.sequel.terms import Term


class Literal(Term):

    def __init__(self, v):
        super().__init__()
        self.v = v


STAR = Literal("*")
DISTINCT = Literal("DISTINCT")
FOR_UPDATE = Literal("FOR UPDATE")
FOR_UPDATE_SHARE = Literal("FOR UPDATE SHARE")
FOR_UPDATE_NOWAIT = Literal("FOR UPDATE NOWAIT")
FOR_UPDATE_SKIP_LOCKED = Literal("FOR UPDATE SKIP LOCKED")


INSERT = Literal("INSERT")
INSERT_IGNORE = Literal("INSERT IGNORE")
REPLACE = Literal("REPLACE")