from sweet.database.driver.base_driver import BaseDriver
from sweet.model import consts
from sweet.model.columns import Column, Columns, Table
from sweet.model.objects import Objects
from sweet.environment import Environment
from sweet.utils import classproperty
from sweet.utils.inflection import tableize


async def init(env: Environment) -> BaseDriver:
    db = env.db_driver(**env.db_settings)
    await db.init_pool()
    setattr(Objects, consts.db_adapter, db)
    setattr(Objects, consts.sql_visitor, env.sql_visitor)
    return db


async def release():
    db = getattr(Objects, consts.db_adapter, None)
    if db is not None:
        await db.close_pool()


class Model:
    __records_class__ = Objects

    @classproperty
    def table(cls) -> Table:
        return getattr(cls, consts.table_name)

    @classproperty
    def columns(cls) -> Columns:
        return cls.table.columns

    @classproperty
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
