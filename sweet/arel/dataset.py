from sweet.arel.nodes.insert_statement import Insert


class DataSet:

    def __init__(self, tablename: str, placeholder: str = '%s'):
        self.placeholder = placeholder
        self.tablename = 'f{"tablename"}'

    def sql(self) -> (str, list[any]):
        pass

    def insert(self, **kwargs) -> Insert:
        return Insert(self.placeholder, **kwargs)



