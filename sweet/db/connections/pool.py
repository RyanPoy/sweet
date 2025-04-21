from __future__ import annotations

import aiosqlite
import asyncio

from sweet.utils.logger import get_logger

logger = get_logger()


class AsyncSet:
    def __init__(self):
        self.data = set()
        self._locker = asyncio.Lock()

    async def add(self, v):
        async with self._locker:
            return self.data.add(v)

    async def remove(self, v):
        async with self._locker:
            return self.data.remove(v)

    async def contains(self, v):
        async with self._locker:
            return v in self.data

    async def clear(self):
        async with self._locker:
            self.data.clear()

    def size(self):
        return len(self.data)


class Pool:

    @classmethod
    async def create(cls, minsize=1, maxsize=10, **kwargs):
        db_path = kwargs.get('db', ':memory:')
        self = cls(minsize=minsize, maxsize=maxsize, db_path=db_path)
        await self._fill_pool()
        return self

    def __init__(self, minsize: int, maxsize: int, db_path: str, **db_config):
        self.db_path = db_path
        self.maxsize = max(minsize, maxsize)
        self._pool = asyncio.Queue(self.maxsize)
        self._closed = False
        self.initialized = False
        self.db_config = db_config
        self.db_config['check_same_thread'] = False

    @property
    def size(self):
        return self._pool.qsize()

    async def _create_conn(self) -> aiosqlite.Connection:
        """创建新连接"""
        return await aiosqlite.connect(self.db_path, check_same_thread=self.db_config['check_same_thread'])

    async def _fill_pool(self):
        """初始化最小连接池"""
        if self.initialized:
            raise RuntimeError("The pool has already been initialized")

        while self.size < self.maxsize:
            conn = await self._create_conn()
            await self._pool.put(conn)
        self.initialized = True

    async def release(self, conn: aiosqlite.Connection):
        await self._pool.put(conn)

    async def acquire(self) -> aiosqlite.Connection:
        """使用标准库的异步上下文管理器"""
        if self._closed:
            raise RuntimeError("Connection pool is closed")
        if not self.initialized:
            raise RuntimeError("Connection pool is not initialized")

        conn = await self._pool.get()
        return conn

    async def close(self):
        """关闭连接池"""
        if self._closed:
            return

        self._closed = True
        # 关闭所有空闲连接
        while not self._pool.empty():
            try:
                conn = self._pool.get_nowait()
                await conn.close()
            except asyncio.QueueEmpty:
                break

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc_info):
        await self.close()
