from asyncio import Queue
from typing import Dict

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
        self.db_config['check_same_thread'] = False
        self.pool = None

    async def init_pool(self, minsize=1, maxsize=10):
        """ initialize connection pool
        """
        self.pool = Queue(maxsize + 2)
        for x in range(maxsize):
            conn = await aiosqlite.connect(self.db_config['db'], check_same_thread=self.db_config['check_same_thread'])
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
            await self.pool.put(connection)

    async def get_connection(self):
        """ get the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection is None:
            connection = await self.pool.get()
            await self.set_autocommit(connection)
            self._local_connection.set(connection)
        return connection

    async def set_autocommit(self, conn, auto=True):
        if auto is True:
            conn.isolation_level = None
        else:
            conn.isolation_level = 'DEFERRED'
        return self

    async def columns(self, table_name: str) -> list[Dict]:
        sql = f"PRAGMA table_info({table_name})"
        rows = await self.fetchall(sql)
        # names = ('cid', 'name', 'type', 'notnull', 'default', 'is_pk')
        return [
            {'name': r[1], 'kind': r[2], 'null': r[3] == 0, 'key': '', 'default': r[4], 'extra': str(r[5])} for r in rows
        ]


