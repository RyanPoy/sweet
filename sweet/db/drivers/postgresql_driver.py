from __future__ import annotations

from contextlib import asynccontextmanager

from sweet.db.drivers.base_driver import BaseDriver
from sweet.utils.logger import get_logger

logger = get_logger()
import copy
import asyncpg


# class PostgreSQLDriver(BaseDriver):
#
#     def __init__(self, **db_config):
#         self.pool = PostgreSQLPool(**db_config)
#
#     async def initialize(self, **db_config) -> Self:
#         await self.pool.initialize()
#         return self
#
#     @asynccontextmanager
#     async def transaction(self):
#         async with self.pool.acquire() as conn:
#             async with conn.transaction():
#                 yield Transaction(conn)
#
#     async def execute_rowid(self, sql: str, *params):
#         async with self.pool.acquire() as conn:
#             row = await conn.fetchrow(sql, *params)
#             return row[0] if row else None
#
#     async def execute_rowids(self, sql: str, params_seq):
#         async with self.pool.acquire() as conn:
#             rows = await conn.executemany(sql, params_seq)
#             return rows
#
#     async def execute_rowcount(self, sql: str, *params):
#         async with self.pool.acquire() as conn:
#             result = await conn.execute(sql, *params)
#             return int(result.split()[-1])
#
#     async def fetchone(self, sql: str, *params: Any) -> dict[str, Any] | None:
#         async with self.pool.acquire() as conn:
#             row = await conn.fetchrow(sql, *params)
#             return dict(row) if row else None
#
#     async def fetchall(self, sql: str, *params: Any) -> list[dict[str, Any]] | None:
#         async with self.pool.acquire() as conn:
#             rows = await conn.fetch(sql, *params)
#             return [dict(r) for r in rows] if rows else None

class PostgreSQLDriver(BaseDriver):

    def __init__(self, **db_config):
        super().__init__()
        self.db_config = copy.copy(db_config)
        self.db_config['dbname'] = self.db_config.pop('db')
        self.pool = None

    async def initialize(self, minsize=1, maxsize=10):
        dsn = ' '.join([f'{k}={v}' for k, v in self.db_config.items()])
        user = self.db_config['user']
        pwd = self.db_config['password']
        dbname = self.db_config['dbname']
        host = self.db_config['host']
        port = self.db_config['port']
        dsn = f'postgres://{user}:{pwd}@{host}:{port}/{dbname}'
        self.pool = await asyncpg.create_pool(dsn, min_size=minsize, max_size=maxsize)
        return self

    async def close_pool(self):
        """ close the connection pool """
        await self._release_connection()
        if self.pool:
            await self.pool.close()

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
        pass

    @asynccontextmanager
    async def transaction(self):
        conn = await self.get_connection()
        async with conn.transaction():
            yield self

    async def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        rows = await self._fetch(sql, *params)
        if not rows:
            return None
        row = rows[0]
        columns = list(row.keys())
        return dict(zip(columns, row))

    async def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        rows = await self._fetch(sql, *params)
        if not rows:
            return None
        columns = list(rows[0].keys())
        return [dict(zip(columns, row)) for row in rows]

    async def execute_lastrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        return await self.execute_returnrowid(sql, *params)

    async def execute_returnrowid(self, sql, *params):
        rows = await self._execute(sql, *params)
        logger.debug("*" * 10)
        logger.debug(type(rows))
        logger.debug("+++" + rows + "+++")
        logger.debug("*" * 10)
        return rows['id']

    async def execute_rowcount(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        rows = await self._execute(sql, *params)
        logger.debug(">" * 10)
        logger.debug(type(rows))
        logger.debug("+++" + rows + "+++")
        logger.debug(">" * 10)
        rowcount = int(rows.split()[-1])
        return rowcount

    async def execute(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        await self._execute(sql, *params)
        return self

    async def _execute(self, sql, *params):
        logger.debug(sql)
        connection = await self.get_connection()
        rows = await connection.execute(sql, *params)
        return rows

    async def _fetch(self, sql, *params):
        connection = await self.get_connection()
        return await connection.fetch(sql, *params)

    async def columns(self, table_name: str) -> list[dict]:
        sql = f"SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns WHERE table_name = '{table_name}'"
        rows = await self.fetchall(sql)
        return [
            {'name': r['column_name'], 'kind': r['data_type'], 'null': r['is_nullable'], 'key': '', 'default': r['column_default'], 'extra': ''} for r in rows
        ]
