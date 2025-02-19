from contextvars import ContextVar
from typing import Dict, List
from sweet.database.driver.base_driver import BaseDriver
import aiomysql


class MySQLDriver(BaseDriver):

    def __init__(self, **db_config):
        """
        kwargs contain:
            db,
            user='root',
            password='',
            host='localhost',
            port=3306,
            charset='utf8',
            show_sql=False
        """
        super().__init__()
        self.db_config = db_config
        self.db_config['init_command'] = "SET sql_mode = 'ANSI_QUOTES';"

        self.pool = None

    async def init_pool(self, minsize=1, maxsize=10):
        """ initialize connection pool
        """
        self.pool = await aiomysql.create_pool(minsize=minsize, maxsize=maxsize, echo=True, **self.db_config)
        return self

    async def close_pool(self):
        """ close the connection pool """
        await self._release_connection()
        if self.pool:
            self.pool.close()
            await self.pool.wait_closed()

    async def _release_connection(self):
        """ release the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection:
            self._local_connection.set(None)
            await self.pool.release(connection)

    async def get_connection(self):
        """ get the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection is None:
            connection = await self.pool.acquire()
            await self.set_autocommit(connection)
            self._local_connection.set(connection)
        return connection

    async def set_autocommit(self, conn, auto=True):
        await conn.autocommit(auto)

    async def columns(self, table_name: str) -> List[Dict]:
        sql = f"SHOW COLUMNS FROM `{table_name}`"
        rows = await self.fetchall(sql)
        # Field | Type | Null | Key | Default | Extra |
        # names = ('name', 'kind', 'null', 'key', 'default', 'extra')
        return [
            {'name': r[0], 'kind': r[1], 'null': r[2], 'key': r[3], 'default': r[4], 'extra': r[5]} for r in rows
        ]
