from enum import Enum

from sweet.model.columns import Column, Columns, Table
from sweet.utils import classproperty, tableize


class Consts:
    table_name = '__table_name__'


class Model:
    @classproperty
    def table(cls) -> Table:
        return getattr(cls, Consts.table_name)

    @classproperty
    def columns(cls) -> Columns:
        return cls.table.columns

    def __init_subclass__(cls, **kwargs):
        col_names = [k for k, v in cls.__dict__.items() if issubclass(type(v), Column)]
        if col_names:
            table = Table(name=tableize(cls.__name__))
            for n in col_names:
                col = getattr(cls, n)
                table.columns.add(n, col)
                delattr(cls, n)
            if Consts.table_name not in cls.__dict__:
                setattr(cls, Consts.table_name, table)

        super().__init_subclass__(**kwargs)
