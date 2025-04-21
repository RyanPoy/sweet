from __future__ import annotations

from contextvars import ContextVar

from sweet.db.connections import Pool
from sweet.db.connections.connection import Connection
from sweet.db.connections.sqlite_connection import SQLiteConnection
from sweet.db.drivers.base_driver import IDriver


class SQLiteDriver(IDriver):

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
        self._local_connection = ContextVar("connection")

    async def initialize(self, minsize=1, maxsize=10):
        self.pool = await Pool.create(minsize, maxsize, **self.db_config)

    async def destroy(self):
        """ close the connection pool """
        await self.release_connection()
        if self.pool:
            await self.pool.close()

    async def get_connection(self):
        """ get the connection of current coroutine """
        conn = self._local_connection.get(None)
        if conn is None:
            conn = await self.pool.acquire()
            conn = SQLiteConnection(conn, self)
            await conn.auto_commit()
            self._local_connection.set(conn)
        return conn

    async def release_connection(self, conn: Connection = None):
        """ release the connection of current coroutine """
        conn = conn or self._local_connection.get(None)
        if conn:
            await self.pool.release(conn.raw_conn())

    async def columns(self, table_name: str) -> list[dict]:
        sql = f"PRAGMA table_info({table_name})"
        rows = await self.fetchall(sql)
        return [
            {'name': r['name'], 'kind': r['type'], 'null': r['notnull'] == 0, 'key': '', 'default': r['dflt_value'], 'extra': str(r['pk'])} for r in rows
        ]
