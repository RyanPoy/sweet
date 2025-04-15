from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from sqlite3 import Connection
from typing import Any, AsyncIterator, Self
import aiosqlite
import aiomysql
import asyncio


class ConnectionPool(ABC):

    @abstractmethod
    async def initialize(self) -> Self: """ initialize connection pool"""

    @abstractmethod
    @asynccontextmanager
    async def acquire(self): """acquire a connection"""

    @abstractmethod
    async def release(self, conn) -> Self: """release the connection to pool"""

    @abstractmethod
    async def close(self) -> Self: """close pool"""


class MySQLPool(ConnectionPool):
    """ MySQL connection pool """

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
        self.db_config = db_config
        self.min_size = db_config.pop("min_size", 2)
        self.max_size = db_config.pop("max_size", 10)
        self.db_config['init_command'] = "SET sql_mode = 'ANSI_QUOTES';"
        self._pool = None

    async def initialize(self, **db_config) -> Self:
        self._pool = await aiomysql.create_pool(minsize=self.min_size, maxsize=self.max_size, echo=True, **self.db_config)
        return self

    async def acquire(self):
        conn = await self._pool.acquire()
        conn.autocommit = True
        return conn

    async def release(self, conn) -> Self:
        await self._pool.release(conn)
        return self

    async def close(self) -> Self:
        if self._pool:
            self._pool.close()
            await self._pool.wait_closed()
        return Self


class PostgreSQLPool(Pool):
    """ MySQL connection pool """

    def __init__(self, **db_config):
        pass

    async def initialize(self) -> Self:
        pass

    async def acquire(self) -> AsyncIterator[Any]:
        pass

    async def release(self, conn) -> Self:
        pass

    async def close(self) -> Self:
        pass


class SQLitePool(Pool):
    """SQLite connection pool"""

    def __init__(self, db_path: str, pool_size: int = 5):
        self.db_path = db_path
        self.pool_size = pool_size
        self._pool = []
        self._in_use = set()

    async def initialize(self) -> Self:
        conn = await aiosqlite.connect(self.db_path)
        for i in range(self.pool_size):
            self._pool.append(conn)
        return self

    async def _create_conn(self) -> Connection:
        return await aiosqlite.connect(self.db_path)

    async def get_connection(self) -> Connection:
        # 复用空闲连接
        while len(self._pool) > 0:
            conn = self._pool.pop()
            if conn not in self._in_use:
                self._in_use.add(conn)
                return conn

        # 等待可用连接
        while True:
            # 实现简单的等待逻辑（生产环境建议使用asyncio队列）
            await asyncio.sleep(0.1)
            if len(self._pool) > 0:
                return self._pool.pop()

    @asynccontextmanager
    async def acquire(self) -> AsyncIterator[Any]:
        conn = await self.get_connection()
        return conn

    async def release(self, conn: Connection) -> None:
        """释放连接"""
        self._in_use.remove(conn)
        self._pool.append(conn)

    async def close(self) -> None:
        """关闭所有连接"""
        for conn in self._pool + list(self._in_use):
            await conn.close()
        self._pool.clear()
        self._in_use.clear()
