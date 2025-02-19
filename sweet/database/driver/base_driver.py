from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from contextvars import ContextVar
from typing import Dict, List

import aiomysql


class BaseDriver(ABC):
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
        self.db_config['init_command'] = "SET sql_mode = 'ANSI_QUOTES';"

        self.pool = None
        self._local_connection = ContextVar('connection')  # 协程局部变量，用于存储连接

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

    async def _get_connection(self):
        """ get the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection is None:
            connection = await self.pool.acquire()
            await self._set_autocommit(connection)
            self._local_connection.set(connection)
        return connection

    async def _set_autocommit(self, conn):
        await conn.autocommit(True)

    async def _cancel_autocommit(self, conn):
        await conn.autocommit(False)

    async def _release_connection(self):
        """ release the connection of current coroutine """
        connection = self._local_connection.get(None)
        if connection:
            self._local_connection.set(None)
            await self.pool.release(connection)

    @asynccontextmanager
    async def transaction(self):
        conn = None
        try:
            conn = await self._get_connection()
            await self._cancel_autocommit(conn)
            yield self
            await conn.commit()
        except Exception as e:
            if conn: await conn.rollback()
            raise
        finally:
            if conn: await self._set_autocommit(conn)

    async def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        async with self.__execute(sql, *params) as cursor:
            return await cursor.fetchone()

    async def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        async with self.__execute(sql, *params) as cursor:
            return await cursor.fetchall()

    async def execute_lastrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        async with self.__execute(sql, *params) as cursor:
            return cursor.lastrowid

    async def execute_rowcount(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        async with self.__execute(sql, *params) as cursor:
            return cursor.rowcount

    async def execute(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        async with self.__execute(sql, *params):
            return self

    @asynccontextmanager
    async def __execute(self, sql, *params):
        connection = await self._get_connection()
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
