from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from contextvars import ContextVar
from typing import Dict, List


class BaseDriver(ABC):
    def __init__(self, **db_config):
        self._local_connection = ContextVar('connection')  # 协程局部变量，用于存储连接

    @abstractmethod
    async def init_pool(self, minsize=1, maxsize=10):
        raise NotImplemented

    @abstractmethod
    async def close_pool(self):
        """ close the connection pool """
        raise NotImplemented

    @abstractmethod
    async def get_connection(self):
        """ get the connection of current coroutine """
        raise NotImplemented

    @abstractmethod
    async def set_autocommit(self, conn, auto=True):
        raise NotImplemented

    @asynccontextmanager
    async def transaction(self):
        conn = None
        try:
            conn = await self.get_connection()
            await self.set_autocommit(conn, False)
            yield self
            await conn.commit()
        except Exception as e:
            if conn: await conn.rollback()
            raise
        finally:
            if conn: await self.set_autocommit(conn)

    async def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        async with self._execute(sql, *params) as cursor:
            return await cursor.fetchone()

    async def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        async with self._execute(sql, *params) as cursor:
            return await cursor.fetchall()

    async def execute_lastrowid(self, sql, *params):
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
        connection = await self.get_connection()
        cursor = None
        try:
            cursor = await connection.cursor()
            await cursor.execute(sql, params)
            yield cursor
        finally:
            if cursor: await cursor.close()

    @abstractmethod
    async def columns(self, table_name: str) -> List[Dict]:
        raise NotImplemented
