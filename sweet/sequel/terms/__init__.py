class Term:

    def sql(self, visitor: "Visitor") -> str:
        c = visitor.visit(self)
        return str(c)
