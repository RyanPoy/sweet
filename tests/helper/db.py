from contextlib import asynccontextmanager
from enum import StrEnum

from sweet import model
from sweet.database.driver import Driver
from sweet.environment import Environment
from tests.helper import User, settings_mysql, settings_postgresql, settings_sqlite


class DB_TYPE(StrEnum):
    mysql = "mysql"
    sqlite = "sqlite"
    pg = "postgresql"


async def init_db(db_type: DB_TYPE) -> Driver:
    match db_type:
        case DB_TYPE.mysql:
            env = Environment(settings_mysql)
        case DB_TYPE.sqlite:
            env = Environment(settings_sqlite)
        case DB_TYPE.pg:
            env = Environment(settings_postgresql)
        case invalid_type:
            raise ValueError(f"Can not support {invalid_type}")
    db = env.db_driver(**env.db_settings)
    await db.init_pool()
    return db


async def close(driver: Driver) -> None:
    await driver.close_pool()


@asynccontextmanager
async def using(env: Environment):
    driver = None
    try:
        driver = await model.init(env)
        yield driver
    finally:
        if driver:
            await driver.close_pool()

