from __future__ import annotations

from contextvars import ContextVar

import aiomysql

from sweet.db.connections import Connection, MySQLConnection
from sweet.db.drivers.base_driver import Driver


class MySQLDriver(Driver):

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

            min_size=1
            max_size=10
        """
        self.db_config = db_config
        self.db_config['init_command'] = "SET sql_mode = 'ANSI_QUOTES';"
        self.minsize = self.db_config.pop('min_size', 1)
        self.maxsize = self.db_config.pop('max_size', 10)
        self.pool = None
        self._local_connection = ContextVar('connection')  # 协程局部变量，用于存储连接

    async def initialize(self):
        self.pool = await aiomysql.create_pool(minsize=self.minsize, maxsize=self.maxsize, echo=True, **self.db_config)

    async def get_connection(self) -> Connection:
        conn = self._local_connection.get(None)
        if conn is None:
            conn = await self.pool.acquire()
            conn = MySQLConnection(conn, self)
            await conn.auto_commit()
            self._local_connection.set(conn)
        return conn

    async def release_connection(self, conn: Connection = None):
        conn = conn or self._local_connection.get(None)
        if conn:
            await self.pool.release(conn.raw_conn())

    async def destroy(self):
        await self.release_connection()
        if self.pool:
            self.pool.close()
            await self.pool.wait_closed()

    async def columns(self, table_name: str) -> list[dict]:
        sql = f"SHOW COLUMNS FROM `{table_name}`"
        rows = await self.fetchall(sql)
        # Field | Type | Null | Key | Default | Extra |
        # names = ('name', 'kind', 'null', 'key', 'default', 'extra')
        return [
            {'name': r['Field'], 'kind': r['Type'], 'null': r['Null'], 'key': r['Key'], 'default': r['Default'], 'extra': r['Extra']} for r in rows
        ]
