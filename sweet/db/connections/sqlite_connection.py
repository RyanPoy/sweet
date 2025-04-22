from __future__ import annotations

from contextlib import asynccontextmanager
from typing import Any, Callable

from sweet.db.connections.connection import Connection
from sweet.db.connections.transaction import Transaction
from sweet.utils.logger import get_logger

logger = get_logger()


class SQLiteConnection(Connection):

    def __init__(self, conn, driver):
        self._raw_conn = conn
        self._driver = driver

    def raw_conn(self):
        return self._raw_conn

    async def auto_commit(self) -> None:
        def _():
            self._raw_conn.isolation_level = "DEFERRED"

        await self._raw_conn._execute(_)

    async def manual_commit(self) -> None:
        def _():
            self._raw_conn.isolation_level = "DEFERRED"

        await self._raw_conn._execute(_)

    async def execute(self, sql: str, *params: Any) -> None:
        async with self._execute_one(sql, *params):
            pass

    async def execute_rowid(self, sql: str, *params: Any) -> int:
        async with self._execute_one(sql, *params) as cur:
            return cur.lastrowid

    async def execute_rowids(self, sql: str, params_seq: [Any]) -> list[int]:
        ids = []
        for params in params_seq:
            if not isinstance(params, (tuple, list)):
                params = [params]
            id = await self.execute_rowid(sql, *params)
            ids.append(id)
        return ids

    async def execute_rowcount(self, sql: str, *params: Any) -> int:
        async with self._execute_one(sql, *params) as cur:
            return cur.rowcount

    async def fetchone(self, sql: str, *params: Any) -> dict | None:
        async with self._execute_one(sql, *params) as cur:
            row = await cur.fetchone()
            if row is None:
                return None
            columns = [col[0] for col in cur.description]
            return dict(zip(columns, row))

    async def fetchall(self, sql: str, *params: Any) -> list[dict]:
        async with self._execute_one(sql, *params) as cur:
            columns = [col[0] for col in cur.description]
            rows = await cur.fetchall()
            return [dict(zip(columns, row)) for row in rows]

    @asynccontextmanager
    async def _execute_one(self, sql: str, *params):
        async def do(cur, sql_str, ps):
            await cur.execute(sql_str, ps)

        async with self._execute(do, sql, params) as cursor:
            yield cursor

    @asynccontextmanager
    async def _execute_many(self, sql: str, param_seq: [Any]):

        async def do(cur, sql_str, ps):
            await cur.executemany(sql_str, ps)

        async with self._execute(do, sql, param_seq) as cursor:
            yield cursor

    @asynccontextmanager
    async def _execute(self, func: Callable, sql, params):
        logger.debug(sql)
        cursor = None
        try:
            cursor = await self._raw_conn.cursor()
            await func(cursor, sql, params)
            yield cursor
        finally:
            if cursor:
                r = cursor.close()
                if r is not None:
                    await r

    async def close(self):
        await self._driver.release_connection(self)

    def transaction(self) -> Transaction:
        return Transaction(self)
