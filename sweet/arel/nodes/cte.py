from sweet.arel.nodes.binary import Binary
from sweet.arel.schema.table import Table


class Cte(Binary):
      # alias :name :left
      # alias :relation :right
    def __init__(self, name, relation, materialized: None):
        # super(self, name, relation)
        self.relation = relation
        self.materialized = materialized

    def __hash__(self):
       return hash(f'{self.name}-{self.relation}-{self.materialized}')

    def __eq__(self, other):
        return self.__class__ == other.__class__ \
            and self.name == other.name \
            and self.relation == other.relation \
            and self.materialized == other.materialized
    alias = __eq__

    def to_table(self):
        return Table(self.name)
