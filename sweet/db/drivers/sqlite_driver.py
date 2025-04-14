from __future__ import annotations

from typing import Any, Self

import aiosqlite
from contextlib import asynccontextmanager

from sweet.db import SQLitePool
from sweet.db.drivers.base_driver import BaseDriver
from sweet.db.transaction import Transaction

#
# class SQLiteDriver(BaseDriver):
#     def __init__(self, **db_config):
#         self.pool = SQLitePool(**db_config)
#
#     async def initialize(self, **db_config) -> Self:
#         await self.pool.initialize()
#         return self
#
#     @asynccontextmanager
#     async def transaction(self):
#         async with aiosqlite.connect(self.db_path) as conn:
#             await conn.execute('BEGIN')
#             try:
#                 yield Transaction(conn)
#                 await conn.commit()
#             except Exception:
#                 await conn.rollback()
#                 raise
#
#     async def execute_rowid(self, sql: str, *params):
#         async with aiosqlite.connect(self.db_path) as conn:
#             async with conn.cursor() as cur:
#                 await cur.execute(sql, params)
#                 return cur.lastrowid
#
#     async def execute_rowids(self, sql: str, params_seq):
#         ids = []
#         async with aiosqlite.connect(self.db_path) as conn:
#             async with conn.cursor() as cur:
#                 for params in params_seq:
#                     await cur.execute(sql, params)
#                     ids.append(cur.lastrowid)
#         return ids
#
#     async def execute_rowcount(self, sql: str, *params):
#         async with aiosqlite.connect(self.db_path) as conn:
#             async with conn.cursor() as cur:
#                 await cur.execute(sql, params)
#                 return cur.rowcount
#
#     async def fetchone(self, sql: str, *params: Any) -> dict[str, Any] | None:
#         async with aiosqlite.connect(self.db_path) as conn:
#             conn.row_factory = aiosqlite.Row
#             async with conn.execute(sql, params) as cur:
#                 row = await cur.fetchone()
#                 return dict(row) if row else None
#
#     async def fetchall(self, sql: str, *params: Any) -> list[dict[str, Any]] | None:
#         async with aiosqlite.connect(self.db_path) as conn:
#             conn.row_factory = aiosqlite.Row
#             async with conn.execute(sql, params) as cur:
#                 rows = await cur.fetchall()
#                 return [dict(r) for r in rows] if rows else None

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

    async def init_pool(self, minsize=1, maxsize=10):
        """ initialize connection pool
        """
        self.pool = Queue(maxsize + 2)
        for x in range(maxsize):
            conn = await aiosqlite.connect(self.db_config['db'], check_same_thread=self.db_config['check_same_thread'])
            await self.pool.put(conn)
        return self

    async def close_pool(self):
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
