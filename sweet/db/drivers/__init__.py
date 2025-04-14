from typing import Type

from sweet.db import Pool
from sweet.db.drivers.base_driver import BaseDriver as Driver
from sweet.db.drivers.mysql_driver import MySQLDriver
from sweet.db.drivers.postgresql_driver import PostgreSQLDriver
from sweet.db.drivers.sqlite_driver import SQLiteDriver

__all__ = ["Driver", "MySQLDriver", "SQLiteDriver", "PostgreSQLDriver"]


def get_driver(db_type: str) -> Type[Driver]:
    """
    根据数据库类型返回相应的数据库连接池
    :param db_type: str 类型，支持 'mysql', 'postgresql', 'sqlite'
    :return: 返回对应的驱动类型
    """
    if db_type == 'mysql':
        return MySQLDriver
    elif db_type == 'postgresql':
        return PostgreSQLDriver
    elif db_type == 'sqlite':
        return SQLiteDriver
    else:
        raise ValueError(f"Unsupported database type: {db_type}")
