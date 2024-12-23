from sweet.sequel.visitors.visitor import Visitor


class PostgreSQLVisitor(Visitor):

    visit_methods_dict = {}
    qchar = '"'


