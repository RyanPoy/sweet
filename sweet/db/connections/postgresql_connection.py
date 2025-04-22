from __future__ import annotations

from typing import Any

from sweet.db.connections.connection import Connection
from sweet.utils.logger import get_logger
from asyncpg.transaction import Transaction as PgTransaction

logger = get_logger()


class PostgreSQLConnection(Connection):
    def __init__(self, conn, driver):
        self._raw_conn = conn
        self._driver = driver

    def raw_conn(self):
        return self._raw_conn

    async def execute(self, sql: str, *params: Any) -> None:
        await self._execute(sql, *params)

    async def execute_rowid(self, sql: str, *params: Any) -> int:  # ok
        rows = await self._fetch(sql, *params)
        return int(rows[-1])

    async def execute_rowids(self, sql: str, params_seq: [Any]) -> list[int]:
        raise NotImplementedError

    async def execute_rowcount(self, sql: str, *params: Any) -> int:  # ok
        rows = await self._execute(sql, *params)
        rowcount = int(rows.split()[-1])
        return rowcount

    async def _execute(self, sql: str, *params: Any):
        logger.debug(sql)
        rows = await self._raw_conn.execute(sql, *params)
        return rows

    async def fetchone(self, sql: str, *params: Any) -> dict | None:  # ok
        rows = await self._raw_conn.fetch(sql, *params)
        if not rows:
            return None
        row = rows[0]
        columns = list(row.keys())
        return dict(zip(columns, row))

    async def fetchall(self, sql: str, *params: Any) -> list[dict]:  # ok
        rows = await self._raw_conn.fetch(sql, *params)
        if not rows:
            return []
        columns = list(rows[0].keys())
        return [dict(zip(columns, row)) for row in rows]

    async def _fetch(self, sql: str, *params: Any) -> list:
        logger.debug(sql)
        rows = await self._raw_conn.fetchrow(sql, *params)
        return rows

    async def close(self):
        await self._driver.release_connection(self)

    def transaction(self) -> PgTransaction:
        return self._raw_conn.transaction()
