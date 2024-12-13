from sweet.arel.nodes.unary import Unary


class Group(Unary):

    def tp(self):
        return "GROUP"

    def is_group(self):
        return True
