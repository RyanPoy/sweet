from dataclasses import dataclass
from typing import List, Self, Tuple, TYPE_CHECKING

if TYPE_CHECKING:
    from sweet.sequel.terms.fn import Fn
    from sweet.sequel.terms.name import Name

from sweet.sequel.quoting import quote
from sweet.sequel.types import Array, ArrayType, V


@dataclass
class Value:
    v: V
    #
    # def __hash__(self):
    #     return hash(str(self.v))
    #


class Values:

    def __init__(self, *args: ArrayType) -> None:
        self.data: List[Array] = []
        self.append(*args)

    def append(self, *args: ArrayType) -> Self:
        for i, x in enumerate(args):
            if not isinstance(x, ArrayType):
                raise TypeError(f"Values class only accepts List or Tuple, but got an {x.__class__.__name__}")
            if i != 0:
                if len(x) != self.data[0].length():
                    raise ValueError("Inconsistent element length")

            self.data.append(Array(x))
        return self

    def is_empty(self):
        return self.data is None or len(self.data) == 0

    def __eq__(self, other: Self) -> bool:
        return self.__class__ == other.__class__ and self.data == other.data
