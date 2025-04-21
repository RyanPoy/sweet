from abc import ABC, abstractmethod
from typing import Any

from sweet.db.connections.transaction import Transaction


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
