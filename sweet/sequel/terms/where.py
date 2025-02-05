from typing import Self

from sweet.sequel.terms.fn import Fn
from sweet.sequel.terms.q import Q


class Filter:
    def __init__(self):
        self.filters: [Q | Fn] = []

    def append(self, *qs: Q | Fn, **kwargs) -> Self:
        for q in qs:
            if isinstance(q, Fn):
                self.filters.append(q)
            elif isinstance(q, Q) and not q.is_empty():
                self.filters.append(q)
        if kwargs:
            self.filters.append(Q(**kwargs))
        return self

    def empty(self):
        return len(self.filters) == 0


class Where(Filter): pass


class Having(Filter): pass
