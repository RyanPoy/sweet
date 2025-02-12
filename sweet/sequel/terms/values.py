from dataclasses import dataclass
from typing import Self, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

from sweet.sequel.quoting import quote
from sweet.sequel.types import V



@dataclass
class Value:

    v: V
    #
    # def __hash__(self):
    #     return hash(str(self.v))
    #

class Values:

    def __init__(self, *args: V) -> None:
        self.vs: Tuple[Value, ...] = tuple(Value(x) for x in args)

    def is_empty(self):
        return self.vs is None or len(self.vs) == 0

    def __len__(self) -> int:
        return len(self.vs)

    def __eq__(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.vs == other.vs


class ValuesList:

    def __init__(self, *args: Values) -> None:
        self.data: [Values] = []
        self.append(*args)

    def is_empty(self):
        return False if self.data else True

    def append(self, *args: Values) -> Self:
        args = [x for x in args if not x.is_empty()]
        self.check_values(*args)
        self.data.extend(args)
        return self

    def check_values(self, *args: Values) -> None:
        if not args:
            return
        len0 = len(args[0])
        for values in args:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")

        for values in self.data:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")
