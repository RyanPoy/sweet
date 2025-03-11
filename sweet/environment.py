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
        db_settings = settings.get(DATABASE, {})
        if not db_settings:
            raise EnvironmentError("'DATABASE' environment does not exists")

        DRIVER = 'DRIVER'
        driver_settings = db_settings.get(DRIVER, None)
        if driver_settings == 'mysql':
            driver_class = MySQLDriver
        elif driver_settings == 'sqlite':
            driver_class = SQLiteDriver
        elif driver_settings == 'postgres':
            driver_class = PostgreSQLDriver
        elif not driver_settings:
            raise EnvironmentError("DATABASE['DRIVER'] environment does not exists")
        else:
            raise EnvironmentError(f"'{driver_settings}' driver is not supported")

        self.driver = driver_class(**driver_settings)
        return self.driver
