from abc import ABC, abstractmethod

from sweet.db.connections.postgresql_connection import Connection
from sweet.utils.logger import get_logger

logger = get_logger()


class Driver(ABC):
    @abstractmethod
    async def initialize(self): """initialize the connection pool"""

    @abstractmethod
    async def destroy(self): """close the connection pool """

    @abstractmethod
    async def get_connection(self) -> Connection: """get a connection"""

    @abstractmethod
    async def release_connection(self, conn: Connection = None): """release a connection"""
