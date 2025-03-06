from sweet.model.columns import Column, Table
from sweet.utils import tableize


class Model:

    def __init_subclass__(cls, **kwargs):
        col_names = [ k for k, v in cls.__dict__.items() if issubclass(type(v), Column) ]
        if col_names:
            table = Table(name=tableize(cls.__name__))
            for n in col_names:
                col = getattr(cls, n)
                setattr(table, n, col)
                delattr(cls, n)
            if 'table' not in cls.__dict__:
                setattr(cls, 'table', table)

        super().__init_subclass__(**kwargs)
