from sweet.sequel.nodes.unary import Unary


class ValuesList(Unary):
    def __init__(self, values: []):
        super().__init__(self)
        self.values = values

    @property
    def rows(self): return self.values

    def to_sql(self):
        """ 为每个值生成 SQL 语句
        :return: 插入 SQL 的片段，例如 ("John", 30)
        """
        return "(" + ', '.join([ v.to_sql() for v in self.values ]) + ")"

    def __eq__(self, other):
        return isinstance(other, ValuesList) and self.values == other.values

    def __hash__(self):
        return hash(f"{self.__class__}{self.values}")
    #
    # def __lshift__(self, other):
    #     self.values.append([other])
    #     return self
