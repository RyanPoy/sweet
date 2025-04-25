from __future__ import annotations
from contextvars import ContextVar
from sweet.db.connections import Connection, PostgreSQLConnection
from sweet.db.drivers.base_driver import Driver
from sweet.utils.logger import get_logger
import asyncpg

logger = get_logger()


class PostgreSQLDriver(Driver):

    def __init__(self, **db_config):
        self.db_config = db_config
        self.db_config['dbname'] = self.db_config.pop('db', None)
        self.minsize = self.db_config.pop('min_size', 1)
        self.maxsize = self.db_config.pop('max_size', 10)
        self.pool = None
        self._local_connection = ContextVar('connection')  # 协程局部变量，用于存储连接

    async def initialize(self):
        user = self.db_config['user']
        pwd = self.db_config['password']
        dbname = self.db_config['dbname']
        host = self.db_config['host']
        port = self.db_config['port']
        dsn = f'postgres://{user}:{pwd}@{host}:{port}/{dbname}'
        self.pool = await asyncpg.create_pool(dsn, min_size=self.minsize, max_size=self.maxsize)

    async def get_connection(self) -> Connection:
        conn = self._local_connection.get(None)
        if conn is None:
            conn = await self.pool.acquire()
            conn = PostgreSQLConnection(conn, self)
            self._local_connection.set(conn)
        return conn

    async def release_connection(self, conn: Connection = None):
        conn = conn or self._local_connection.get(None)
        if conn:
            await self.pool.release(conn.raw_conn())

    async def destroy(self):
        await self.release_connection()
        if self.pool:
            await self.pool.close()

    # async def columns(self, table_name: str) -> list[dict]:
    #     sql = f"SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns WHERE table_name = '{table_name}'"
    #     rows = await self.fetchall(sql)
    #     return [
    #         {'name': r['column_name'], 'kind': r['data_type'], 'null': r['is_nullable'], 'key': '', 'default': r['column_default'], 'extra': ''} for r in rows
    #     ]
