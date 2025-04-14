from __future__ import annotations

from contextlib import asynccontextmanager
from typing import Any

from sweet.db.drivers.base_driver import BaseDriver
from sweet.db.transaction import BaseTransaction

import aiomysql


class MySQLDriver(BaseDriver):
    def __init__(self, pool: aiomysql.Pool):
        self.pool = pool

    @asynccontextmanager
    async def transaction(self):
        async with self.pool.acquire() as conn:
            async with conn.cursor() as cur:
                await conn.begin()
                try:
                    yield BaseTransaction(conn)
                    await conn.commit()
                except Exception:
                    await conn.rollback()
                    raise

    async def execute_rowid(self, sql: str, *params):
        async with self.pool.acquire() as conn:
            async with conn.cursor() as cur:
                await cur.execute(sql, params)
                return cur.lastrowid

    async def execute_rowids(self, sql: str, params_seq):
        ids = []
        async with self.pool.acquire() as conn:
            async with conn.cursor() as cur:
                for params in params_seq:
                    await cur.execute(sql, params)
                    ids.append(cur.lastrowid)
        return ids

    async def execute_rowcount(self, sql: str, *params):
        async with self.pool.acquire() as conn:
            async with conn.cursor() as cur:
                count = await cur.execute(sql, params)
                return count

    async def fetchone(self, sql: str, *params: Any) -> dict[str, Any] | None:
        async with self.pool.acquire() as conn:
            async with conn.cursor(aiomysql.DictCursor) as cur:
                await cur.execute(sql, params)
                row = await cur.fetchone()
                return row  # already dict or None

    async def fetchall(self, sql: str, *params: Any) -> list[dict[str, Any]] | None:
        async with self.pool.acquire() as conn:
            async with conn.cursor(aiomysql.DictCursor) as cur:
                await cur.execute(sql, params)
                rows = await cur.fetchall()
                return list(rows) if rows else None

# # from sweet.db.drivers.base_driver import BaseDriver
# # import aiomysql
#
#
# class MySQLDriver(BaseDriver):
#
#     def __init__(self, **db_config):
#         """
#         kwargs contain:
#             db,
#             user='root',
#             password='',
#             host='localhost',
#             port=3306,
#             charset='utf8',
#             show_sql=False
#         """
#         super().__init__()
#         self.db_config = db_config
#         self.db_config['init_command'] = "SET sql_mode = 'ANSI_QUOTES';"
#
#         self.pool = None
#
#     async def init_pool(self, minsize=1, maxsize=10):
#         """ initialize connection pool
#         """
#         self.pool = await aiomysql.create_pool(minsize=minsize, maxsize=maxsize, echo=True, **self.db_config)
#         return self
#
#     async def close_pool(self):
#         """ close the connection pool """
#         await self._release_connection()
#         if self.pool:
#             self.pool.close()
#             await self.pool.wait_closed()
#
#     async def _release_connection(self):
#         """ release the connection of current coroutine """
#         connection = self._local_connection.get(None)
#         if connection:
#             self._local_connection.set(None)
#             await self.pool.release(connection)
#
#     async def get_connection(self):
#         """ get the connection of current coroutine """
#         connection = self._local_connection.get(None)
#         if connection is None:
#             connection = await self.pool.acquire()
#             await self.set_autocommit(connection)
#             self._local_connection.set(connection)
#         return connection
#
#     async def set_autocommit(self, conn, auto=True):
#         await conn.autocommit(auto)
#
#     async def columns(self, table_name: str) -> list[dict]:
#         sql = f"SHOW COLUMNS FROM `{table_name}`"
#         rows = await self.fetchall(sql)
#         # Field | Type | Null | Key | Default | Extra |
#         # names = ('name', 'kind', 'null', 'key', 'default', 'extra')
#         return [
#             {'name': r['Field'], 'kind': r['Type'], 'null': r['Null'], 'key': r['Key'], 'default': r['Default'], 'extra': r['Extra']} for r in rows
#         ]
