from __future__ import annotations

from asyncio import Queue

from sweet.db.drivers.base_driver import BaseDriver
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

    async def initialize(self, minsize=1, maxsize=10):
        """ initialize connection pool
        """
        self.pool = Queue(maxsize + 2)
        for x in range(maxsize):
            conn = await aiosqlite.connect(self.db_config['db'], check_same_thread=self.db_config['check_same_thread'])
            await self.pool.put(conn)
        return self

    async def destroy(self):
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

    async def columns(self, table_name: str) -> list[dict]:
        sql = f"PRAGMA table_info({table_name})"
        rows = await self.fetchall(sql)
        return [
            {'name': r['name'], 'kind': r['type'], 'null': r['notnull'] == 0, 'key': '', 'default': r['dflt_value'], 'extra': str(r['pk'])} for r in rows
        ]

    async def execute_returnrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        async with self._execute(sql, *params) as cursor:
            row = await cursor.fetchone()
            return row[0]  # 打印返回的 id
