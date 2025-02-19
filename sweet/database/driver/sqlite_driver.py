from asyncio import Queue
from typing import Dict, List

from sweet.database.driver.base_driver import BaseDriver
import aiosqlite


class SQLiteDriver(BaseDriver):
    def __init__(self, **db_config):
        """
        kwargs contain:
            db,
            charset='utf8',
            show_sql=False
        """
        super().__init__()
        self.db_config = db_config
        self.pool = None
        self.db_name = db_config.get('db')

    async def init_pool(self, minsize=1, maxsize=10):
        """ initialize connection pool
        """
        self.pool = Queue(maxsize + 2)
        for x in range(maxsize):
            conn = await aiosqlite.connect(self.db_name)
            await self.pool.put(conn)
        return self

    async def close_pool(self):
        """ close the connection pool """
        await self._release_connection()
        if self.pool:
            while not self.pool.empty():
                conn = await self.pool.get()
                await conn.close()

    async def _release_connection(self):
        """ release the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection:
            self._local_connection.set(None)
            await self.pool.release(connection)

    async def _get_connection(self):
        """ get the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection is None:
            connection = await self.pool.get()
            await self._set_autocommit(connection)
            self._local_connection.set(connection)
        return connection

    async def columns(self, table_name: str) -> List[Dict]:
        pass
