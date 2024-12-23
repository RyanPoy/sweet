from sweet.sequel.visitors.visitor import Visitor


class SQLiteVisitor(Visitor):

    visit_methods_dict = {}
    qchar = '"'


