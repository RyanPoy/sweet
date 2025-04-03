from sweet.model import consts
from sweet.model.columns import Column, Table
from sweet.model.objects import Objects
from sweet.utils import class_property
from sweet.utils.inflection import tableize


async def release():
    db = getattr(Objects, consts.db_adapter, None)
    if db is not None:
        await db.close_pool()


class Model:
    __records_class__ = Objects

    # noinspection PyMethodParameters
    @class_property
    def table(cls) -> Table:
        return getattr(cls, consts.table_name)

    # noinspection PyMethodParameters
    @class_property
    def column_names(cls) -> [str]:
        if not hasattr(cls, "__column_names__"):
            cls.__column_names__ = []
            for col_name, _ in cls.table.columns:
                cls.__column_names__.append(col_name)
        return cls.__column_names__

    # noinspection PyMethodParameters
    @class_property
    def objects(cls):
        return cls.__records_class__(cls)

    def __init_subclass__(cls, **kwargs):
        col_names = [k for k, v in cls.__dict__.items() if issubclass(type(v), Column)]
        if col_names:
            table = Table(name=tableize(cls.__name__))
            for n in col_names:
                col = getattr(cls, n)
                table.columns.add(n, col)
                delattr(cls, n)
            if consts.table_name not in cls.__dict__:
                setattr(cls, consts.table_name, table)

        super().__init_subclass__(**kwargs)

    def __init__(self, **kwargs):
        for k, v in kwargs.items():
            setattr(self, k, v)

    def dict(self) -> dict:
        return {col_name: getattr(self, col_name, None) for col_name, _ in self.table.columns}
