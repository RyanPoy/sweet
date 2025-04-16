from __future__ import annotations

from abc import ABC, abstractmethod
from contextlib import asynccontextmanager
from typing import Any

from sweet.db.transaction import Transaction
from sweet.utils.logger import get_logger
from asyncpg.transaction import Transaction as PgTransaction

logger = get_logger()


class Connection(ABC):

    async def raw(self, sql: str, *params: Any) -> None:
        return await self.execute(sql, *params)

    @abstractmethod
    def raw_conn(self): """get raw connection"""

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

    @abstractmethod
    def transaction(self) -> Transaction: """create a transaction"""
    # def __await__(self):
    #     return self
    #
    # async def __aenter__(self):
    #     return self
    #
    # async def __aexit__(self, exc_type, exc_val, exc_tb):
    #     return await self.close()


class MySQLConnection(Connection):

    def __init__(self, conn, driver):
        self._raw_conn = conn
        self._driver = driver

    def raw_conn(self):
        return self._raw_conn

    async def auto_commit(self) -> None:
        await self._raw_conn.autocommit(True)

    async def manual_commit(self) -> None:
        await self._raw_conn.autocommit(False)

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
            cursor = await self._raw_conn.cursor()
            await cursor.execute(sql, params)
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


class PostgreSQLConnection(Connection):
    def __init__(self, conn, driver):
        self._raw_conn = conn
        self._driver = driver

    def raw_conn(self):
        return self._raw_conn

    async def execute(self, sql: str, *params: Any) -> None:
        await self._execute(sql, *params)

    async def execute_rowid(self, sql: str, *params: Any) -> int:  # ok
        rows = await self._execute(sql, *params)
        return rows['id']

    async def execute_rowids(self, sql: str, params_seq: [Any]) -> list[int]:
        raise NotImplementedError

    async def execute_rowcount(self, sql: str, *params: Any) -> int:  # ok
        rows = await self._execute(sql, *params)
        rowcount = int(rows.split()[-1])
        return rowcount

    async def _execute(self, sql: str, *params: Any):
        logger.debug("*" * 10)
        logger.debug(sql)
        rows = await self._raw_conn.execute(sql, *params)
        logger.debug(type(rows))
        logger.debug("+++" + rows + "+++")
        logger.debug("*" * 10)
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

    async def close(self):
        await self._driver.release_connection(self)

    def transaction(self) -> PgTransaction:
        return self._raw_conn.transaction()
