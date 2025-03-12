from sweet.database.driver import MySQLDriver
from sweet.database.driver.postgresql_driver import PostgreSQLDriver
from sweet.database.driver.sqlite_driver import SQLiteDriver


class Environment:

    def __init__(self, settings):
        self.settings = settings
        self.db_driver = None
        self.db_settings = None

        self._init_db()

    def _init_db(self):
        DATABASE = 'DATABASE'
        db_settings = getattr(self.settings, DATABASE, {})
        if not db_settings:
            raise EnvironmentError(f"'{DATABASE}' environment does not exists")
        DRIVER = 'driver'
        match db_settings.get(DRIVER, None):
            case 'mysql':
                driver_class = MySQLDriver
            case 'sqlite':
                driver_class = SQLiteDriver
            case 'postgresql':
                driver_class = PostgreSQLDriver
            case None:
                raise EnvironmentError(f"DATABASE['{DRIVER}'] environment does not exists")
            case invalid_driver:
                raise EnvironmentError(f"'{invalid_driver}' driver is not supported")

        self.db_driver = driver_class
        self.db_settings = db_settings.copy()
        self.db_settings.pop(DRIVER)
