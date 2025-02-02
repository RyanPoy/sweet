from dataclasses import dataclass
from typing import Self


@dataclass
class Literal:
    v: str


STAR = Literal("*")
DISTINCT = Literal("DISTINCT")

INSERT = Literal("INSERT")
INSERT_IGNORE = Literal("INSERT IGNORE")
REPLACE = Literal("REPLACE")
