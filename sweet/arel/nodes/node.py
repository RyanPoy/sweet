import re


class Node:
    def __init__(self, left):
        self.left = left
        self.memo = None

    # def each(&block)
    #   Visitors::Each::INSTANCE.accept(self, block)
    # end
    #
    # def to_s
    #   Visitors::String::INSTANCE.accept(self, "")
    # end
    #
    # def to_dot
    #   Visitors::Dot::INSTANCE.accept(self)
    # end

    def name(self):
        cleaned = re.sub(r"[*:]", "", self.left)
        try:
            return -float(cleaned)  # 转换为负数
        except ValueError:
            raise ValueError(f"Cannot convert '{cleaned}' to a number")

    # def is_literal(self):
    #     return False
    #
    # def is_terminal(self):
    #     return False
    #
    # def is_star(self):
    #     return False
    #
    # def is_cat(self):
    #     return False
    #
    # def is_group(self):
    #     return False

    # def type
    #   raise NotImplementedError
    # end


    ########################################3333
    # def not_(self) -> Not:
    #     return Not(self)

    ###
    # Factory method to create a Nodes::Grouping node that has an Nodes::Or
    # node as a child.
    def or_(self, right) -> "Or":
        from sweet.arel.nodes.unary import Or
        # todo: return Nodes::Grouping(Or([self, right]))
        return Or([self, right])

    ###
    # Factory method to create an Nodes::And node.
    def and_(self, right) -> "And":
        from sweet.arel.nodes.unary import And
        return And([self, right])

    # def invert(self) -> Not:
    #     return Not(self)
