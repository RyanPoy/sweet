from __future__ import annotations

from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from typing import Any

from sweet.utils.logger import get_logger

logger = get_logger()


class BaseConnection(ABC):

    async def raw(self, sql: str, *params: Any) -> None:
        return await self.execute(sql, *params)

    @abstractmethod
    async def execute(self, sql: str, *params: Any) -> None: """execute a sql statement"""

    @abstractmethod
    async def execute_rowid(self, sql: str, *params: Any) -> int: """execute a sql statement and return the last rowid"""

    @abstractmethod
    async def execute_rowids(self, sql: str, params_seq: [Any]) -> list[int]: """execute a batch of sql statements and return a list of last rowid"""

    @abstractmethod
    async def execute_rowcount(self, sql: str, *params: Any) -> int: """execute a sql statement and return the number of rows affected"""

    @abstractmethod
    async def fetchone(self, sql: str, *params: Any) -> dict | None: """fetch a single record from the database"""

    @abstractmethod
    async def fetchall(self, sql: str, *params: Any) -> list[dict]: """fetch all records from the database"""

    @abstractmethod
    async def close(self) -> list[dict]: """close the connection"""


class MySQLConnection(BaseConnection):

    def __init__(self, conn, driver):
        self._conn = conn
        self._driver = driver

    async def execute(self, sql: str, *params: Any) -> None:
        async with self._execute(sql, *params):
            pass

    async def execute_rowid(self, sql: str, *params: Any) -> int:
        async with self._execute(sql, *params) as cur:
            return cur.lastrowid

    async def execute_rowids(self, sql: str, params_seq: [Any]) -> list[int]:
        raise NotImplementedError

    async def execute_rowcount(self, sql: str, *params: Any) -> int:
        async with self._execute(sql, *params) as cur:
            return cur.rowcount

    async def fetchone(self, sql: str, *params: Any) -> dict | None:
        async with self._execute(sql, *params) as cur:
            row = await cur.fetchone()
            if row is None:
                return None
            columns = [col[0] for col in cur.description]
            return dict(zip(columns, row))

    async def fetchall(self, sql: str, *params: Any) -> list[dict]:
        async with self._execute(sql, *params) as cur:
            columns = [col[0] for col in cur.description]
            rows = await cur.fetchall()
            return [dict(zip(columns, row)) for row in rows]

    @asynccontextmanager
    async def _execute(self, sql: str, *params):
        logger.debug(sql)
        cursor = None
        try:
            cursor = await self._conn.cursor()
            await cursor.execute(sql, params)
            yield cursor
        finally:
            if cursor:
                r = cursor.close()
                if r is not None:
                    await r

    async def close(self):
        self._driver.release(self)
