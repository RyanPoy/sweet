from typing import Self

from sweet.sequel.terms.fn import Fn
from sweet.sequel.terms.pair import Pair
from sweet.sequel.terms.q import Q


class Where:

    def __init__(self):
        self.qs: [Q | Fn] = []

    def append(self, *qs: Q | Fn, **kwargs) -> Self:
        for q in qs:
            if isinstance(q, Fn):
                self.qs.append(q)
            elif isinstance(q, Q) and not q.is_empty():
                self.qs.append(q)
        if kwargs:
            self.qs.append(Q(**kwargs))
        return self

    def empty(self):
        return len(self.qs) == 0
