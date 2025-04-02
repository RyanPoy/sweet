import pytest

from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class ObjDict(dict):

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(f"'{self.__class__.__name__}' object has no attribute '{key}'")

    def __setattr__(self, key, value):
        self[key] = value


@pytest.fixture
def visitors():
    return ObjDict(
        mysql=MySQLVisitor(),
        sqlite=SQLiteVisitor(),
        pg=PostgreSQLVisitor(),
    )
