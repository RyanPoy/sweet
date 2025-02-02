from enum import Enum


class Term:

    def sql(self, visitor: "Visitor") -> str:
        c = visitor.visit(self)
        return str(c)


class Logic(Enum):
    AND = 'AND'
    OR = 'OR'

    def __str__(self):
        return self.value

