from typing import Self

from sweet.drivers import MySQLDriver
from sweet.drivers import PostgreSQLDriver
from sweet.drivers import SQLiteDriver
from sweet.models import Objects
from sweet import consts
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class Environment:

    def __init__(self, settings):
        self.settings = settings
        self.db_driver = None
        self.db_settings = None
        self.sql_visitor = None

        self.db = None
        self._init_db_settings()

    def _init_db_settings(self):
        DATABASE = 'DATABASE'
        db_settings = getattr(self.settings, DATABASE, {})
        if not db_settings:
            raise EnvironmentError(f"'{DATABASE}' environment does not exists")
        DRIVER = 'drivers'
        match db_settings.get(DRIVER, None):
            case 'mysql':
                driver_class = MySQLDriver
                sql_visitor = MySQLVisitor
            case 'sqlite':
                driver_class = SQLiteDriver
                sql_visitor = SQLiteVisitor
            case 'postgresql':
                driver_class = PostgreSQLDriver
                sql_visitor = PostgreSQLVisitor
            case None:
                raise EnvironmentError(f"DATABASE['{DRIVER}'] environment does not exists")
            case invalid_driver:
                raise EnvironmentError(f"'{invalid_driver}' drivers is not supported")

        self.sql_visitor = sql_visitor
        self.db_driver = driver_class
        self.db_settings = db_settings.copy()
        self.db_settings.pop(DRIVER)

    async def init_db(self) -> Self:
        self.db = self.db_driver(**self.db_settings)
        await self.db.init_pool()
        setattr(Objects, consts.db_adapter, self.db)
        setattr(Objects, consts.sql_visitor, self.sql_visitor)
        return self

    async def release_db(self) -> Self:
        if self.db is not None:
            await self.db.close_pool()
        return self
