from __future__ import annotations

import copy
from dataclasses import dataclass
from typing import Self, Union

from sweet.sequel.terms.ext_key import ExtKey
from sweet.sequel.terms.literal import Literal, STAR


@dataclass
class Name(ExtKey):
    schema_name: str = None

    def __post_init__(self) -> None:
        if isinstance(self.schema_name, Name):
            self.schema_name = self.schema_name.alias if self.schema_name.alias else self.schema_name.name

    def __eq__(self, other: Union[Self | Literal | str]) -> bool:
        if isinstance(other, str) and other == '*' and self.name == '*':
            return True
        if isinstance(other, Literal) and other == STAR and self.name == '*':
            return True
        if self.__class__ != other.__class__:
            return False
        return self.name == other.name and self.schema_name == other.schema_name and self.alias == other.alias
