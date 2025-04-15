from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from contextvars import ContextVar

from sweet.db.connection import Connection
from sweet.utils.logger import get_logger

logger = get_logger()


class IDriver(ABC):
    @abstractmethod
    async def initialize(self): """initialize the connection pool"""

    @abstractmethod
    async def destroy(self): """close the connection pool """

    @abstractmethod
    async def get_connection(self) -> Connection: """get a connection"""

    @abstractmethod
    async def release_connection(self, conn: Connection = None): """release a connection"""


class BaseDriver(ABC):
    def __init__(self, **db_config):
        self._local_connection = ContextVar('connection')  # 协程局部变量，用于存储连接

    @abstractmethod
    async def initialize(self, minsize=1, maxsize=10):
        """initialize the connection pool"""

    @abstractmethod
    async def destroy(self):
        """close the connection pool """

    @abstractmethod
    async def get_connection(self):
        """get the connection of current coroutine from pool """

    @abstractmethod
    async def set_autocommit(self, conn, auto=True):
        raise NotImplementedError

    @asynccontextmanager
    async def transaction(self):
        conn = None
        try:
            conn = await self.get_connection()
            await self.set_autocommit(conn, False)
            yield self
            await conn.commit()
        except Exception:
            if conn: await conn.rollback()
            raise
        finally:
            if conn: await self.set_autocommit(conn)

    async def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        async with self._execute(sql, *params) as cursor:
            row = await cursor.fetchone()
            if row is None:
                return None
            columns = [col[0] for col in cursor.description]
            return dict(zip(columns, row))

    async def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        async with self._execute(sql, *params) as cursor:
            columns = [col[0] for col in cursor.description]
            rows = await cursor.fetchall()
            return [dict(zip(columns, row)) for row in rows]

    async def execute_lastrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        async with self._execute(sql, *params) as cursor:
            return cursor.lastrowid

    async def execute_returnrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        async with self._execute(sql, *params) as cursor:
            return cursor.lastrowid

    async def execute_rowcount(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        async with self._execute(sql, *params) as cursor:
            return cursor.rowcount

    async def execute(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        async with self._execute(sql, *params):
            return self

    @asynccontextmanager
    async def _execute(self, sql, *params):
        logger.debug(sql)
        connection = await self.get_connection()
        cursor = None
        try:
            cursor = await connection.cursor()
            await cursor.execute(sql, params)
            yield cursor
        finally:
            if cursor:
                r = cursor.close()
                if r is not None:
                    await r

    @abstractmethod
    async def columns(self, table_name: str) -> list[dict]:
        raise NotImplementedError
