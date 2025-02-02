from sweet.sequel.terms.literal import Literal
from sweet.sequel.terms.name import Name


class Lock:

    def __init__(self, share: bool = False, nowait: bool = False, skip: bool = False, of: (str,) = ()) -> None:
        super().__init__()
        self.prefix = Literal("FOR UPDATE")
        if share:
            self.suffix = Literal("SHARE")
        elif nowait:
            self.suffix = Literal("NOWAIT")
        elif skip:
            self.suffix = Literal("SKIP LOCKED")
        else:
            self.suffix = ""
        self.ofs = [ Name(o) for o in of ] if of else []
