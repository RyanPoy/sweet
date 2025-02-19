import os.path

from sweet.database.driver import Driver, MySQLDriver
from sweet.database.driver.sqlite_driver import SQLiteDriver


async def init_mysql() -> Driver:
    mysql = MySQLDriver(**{
        "host"    : "127.0.0.1",
        "port"    : 3306,
        "user"    : "root",
        "password": "",
        "db"      : "sweet",
    })
    await mysql.init_pool()
    return mysql


async def init_sqlite() -> Driver:
    db_name = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', 'sweet.sqlite'))
    sqlite = SQLiteDriver(**{
        "db": db_name,
    })
    await sqlite.init_pool()
    return sqlite


async def close(driver: Driver) -> None:
    try:
        await driver.close_pool()
    except:
        pass
