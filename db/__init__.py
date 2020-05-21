#coding: utf8
from .mysql import MySQL


class DBManager(object):
    """
    config: {
        'default': {
            'driver': 'mysql',
            'host': 'localhost',
            'port': 3306,
            'database': 'sweet_test',
            'user': 'root',
            'password': '',
            'show_sql': True,
        },
        'reader': {
            xxx: xxxx
        },
        'writer': {
            xxx: xxxx
        },
    }
    """
    DEFAULT_NAME = 'default'
    DRIVER_MAPPING = {
        'mysql': MySQL,
    }

    def __init__(self, config):
        self.config = config
        self.check()

    def check(self):
        for name, db_config in self.config.items():
            driver_name = db_config.get('driver', None)
            if not driver_name:
                raise Exception("Missing driver config in database '%s'" % name)

            driver_class = self.DRIVER_MAPPING.get(driver_name, None)
            if not driver_class:
                raise Exception("Can not support '%s' driver" % driver_name)

        return self

    def get_config_with(self, name=None):
        name = name or self.DEFAULT_NAME
        return self.config.get(name, None)

    def new_db(self, name=None):
        name = name or self.DEFAULT_NAME
        db_config = self.get_config_with(name)
        if not db_config:
            raise Exception("Can not found database config named '%s'" % name)

        driver_name = db_config.get('driver', None)
        driver_class = self.DRIVER_MAPPING.get(driver_name, None)

        return driver_class(**db_config)


__all__ = [DBManager, MySQL]
