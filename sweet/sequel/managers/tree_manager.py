from sweet.sequel.visitor.visitor import Visitor
from sweet.utils import is_array


class TreeManager:
    # module StatementMethods
    def __init__(self):
        self.ast = None

    def take(self, limit):
        if limit:
            self.ast.limit = Limit(limit)
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
            self.key = value # Todo: self.key = [ quote(v) for v in value ]
        else:
            self.key = value # Todo: self.key = quote(value)
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
    #
    # def initialize_copy(other)
    #   super
    #   @ast = @ast.clone
    # end