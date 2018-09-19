#coding: utf8
from sweet.database import MySQL


class DBManager(object):
    """
    {
        'driver': 'mysql',
        'host': 'localhost',
        'port': 3306,
        'database': 'oratordemo',
        'user': 'root',
        'password': '',
        'show_sql': True,
    }
    """
    DRIVER_DB_MAPPING = {
        'mysql': MySQL,
    }

    __connections__ = {}

    def __init__(self, config):
        self.driver = config['driver'].lower()
        self.host = config.get('host', 'localhost')
        self.port = int(config['port'])
        self.database = config['database']
        self.user = config['user']
        self.password = config.get('password', '')
        self.show_sql = config.get('show_sql', False)
        self.db_class = self.DRIVER_DB_MAPPING.get(self.driver)

    def get_connection(self):
        if self.driver not in self.__class__.__connections__:
            self.__class__.__connections__[self.driver] = self.db_class(
                self.database, user=self.user, password=self.password, 
                host=self.host, port=self.port, charset='utf8', show_sql=self.show_sql
            )
        return self.__class__.__connections__[self.driver]

    def new_connection(self):
        return self.db_class(
            self.database, user=self.user, password=self.password, 
            host=self.host, port=self.port, charset='utf8', show_sql=self.show_sql
        )

    def close_all_connection(self):
        for driver, conn in self.__connections__.iteritems():
            conn.close()
        return self

