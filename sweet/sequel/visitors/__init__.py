from typing import Type

from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor
from sweet.sequel.visitors.visitor import Visitor


def get_visitor(db_type: str) -> Type[Visitor]:
    """
    根据数据库类型返回相应的驱动
    :param db_type: str 类型，支持 'mysql', 'postgresql', 'sqlite'
    :return: 返回对应的驱动类型
    """
    if db_type == 'mysql':
        return MySQLVisitor
    elif db_type == 'postgresql':
        return PostgreSQLVisitor
    elif db_type == 'sqlite':
        return SQLiteVisitor
    else:
        raise ValueError(f"Unsupported database type: {db_type}")


__all__ = ["Visitor", "MySQLVisitor", "SQLiteVisitor", "PostgreSQLVisitor", "get_visitor"]
