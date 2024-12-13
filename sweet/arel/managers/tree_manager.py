from sweet.arel.nodes.unary import Limit, Offset, Quoted
from sweet.arel.visitor.visitor import Visitor
from sweet.utils import is_array


class TreeManager:
    # module StatementMethods
    def __init__(self):
        self.ast = None

    def take(self, limit):
        if limit:
            self.ast.limit = Limit(Quoted(limit))
        return self

    def offset(self, offset):
        if offset:
            self.ast.offset = Offset(offset)
        return self

    def order(self, *expr):
        self.ast.orders = expr
        return self

    @property
    def key(self):
        return self.ast.key

    @key.setter
    def key(self, value):
        if is_array(value):
            self.ast.key = [ Quoted(v) for v in value ]
        else:
            self.ast.key = Quoted(value)

    @property
    def wheres(self):
        return self.ast.wheres

    @wheres.setter
    def wheres(self, exprs):
        self.ast.wheres = exprs

    def where(self, expr):
        self.ast.wheres.append(expr)
        return self

    # attr_reader :ast
    #
    # def to_dot
    #   collector = Arel::Collectors::PlainString.new
    #   collector = Visitors::Dot.new.accept @ast, collector
    #   collector.value
    # end
    #
    def to_sql(self, visitor: Visitor):
        return str(visitor.visit(self.ast))
