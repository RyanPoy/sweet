from dataclasses import dataclass

from sweet.database.driver import MySQLDriver
from sweet.database.driver.postgresql_driver import PostgreSQLDriver
from sweet.database.driver.sqlite_driver import SQLiteDriver


@dataclass
class Environment:

    def __init__(self, **settings):
        self.driver = None


    def init_db(self, **settings):
        DATABASE = 'DATABASE'
        db = settings.get(DATABASE, {})
        if not db:
            raise EnvironmentError("'DATABASE' environment does not exists")

        DRIVER = 'DRIVER'
        driver = db.get(DRIVER, None)
        if driver == 'mysql':
            driver_class = MySQLDriver()
        elif driver == 'sqlite':
            driver_class = SQLiteDriver()
        elif driver == 'postgres':
            driver_class = PostgreSQLDriver()
        elif not driver:
            raise EnvironmentError("DATABASE['DRIVER'] environment does not exists")
        else:
            raise EnvironmentError(f"'{driver}' driver is not supported")
        return self.driver
