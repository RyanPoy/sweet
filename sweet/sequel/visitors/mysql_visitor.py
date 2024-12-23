from sweet.sequel.visitors.visitor import Visitor


class MySQLVisitor(Visitor):

    visit_methods_dict = {}
    qchar = '`'
