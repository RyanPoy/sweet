import copy
from typing import Type

from sweet.db.drivers.base_driver import BaseDriver as Driver
from sweet.db.drivers.mysql_driver import MySQLDriver
from sweet.db.drivers.postgresql_driver import PostgreSQLDriver
from sweet.db.drivers.sqlite_driver import SQLiteDriver

__all__ = ["Driver", "MySQLDriver", "SQLiteDriver", "PostgreSQLDriver"]


async def get_driver(**db_settings) -> Driver:
    """
    根据数据库类型返回相应的数据库连接池
    :param db_type: str 类型，支持 'mysql', 'postgresql', 'sqlite'
    :return: 返回对应的驱动类型
    """
    DRIVER = 'driver'
    db_type = db_settings.get(DRIVER)
    driver = None
    if db_type == 'mysql':
        driver = MySQLDriver
    elif db_type == 'postgresql':
        driver = PostgreSQLDriver
    elif db_type == 'sqlite':
        driver = SQLiteDriver
    else:
        raise ValueError(f"Unsupported database type: {db_type}")
    new_db_settings = copy.copy(db_settings)
    new_db_settings.pop(DRIVER)
    db = driver(**new_db_settings)
    await db.initialize()
    return db
