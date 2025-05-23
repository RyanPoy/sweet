from typing import Self

from sweet.db.drivers import get_driver
from sweet.models import Objects
from sweet import consts
from sweet.sequel.visitors import get_visitor


class Environment:

    def __init__(self, settings):
        self.settings = settings
        self.db_settings = None
        self.sql_visitor = None

        self.db = None
        self._init_db_settings()

    def _init_db_settings(self):
        DATABASE = 'DATABASE'
        db_settings = getattr(self.settings, DATABASE, {})
        if not db_settings:
            raise EnvironmentError(f"'{DATABASE}' environment does not exists")
        DRIVER = 'driver'
        db_type = db_settings.get(DRIVER)
        if db_type is None:
            raise EnvironmentError(f"DATABASE['{DRIVER}'] environment does not exists")
        self.sql_visitor = get_visitor(db_type)
        self.db_settings = db_settings

    async def init_db(self) -> Self:
        self.db = await get_driver(**self.db_settings)
        setattr(Objects, consts.db_adapter, self.db)
        setattr(Objects, consts.sql_visitor, self.sql_visitor)
        return self

    async def release_db(self) -> Self:
        if self.db is not None:
            await self.db.destroy()
        return self
