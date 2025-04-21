import inspect


class Transaction:

    def __init__(self, conn):
        self.conn = conn

    async def __aenter__(self):
        await self.start()

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        if exc_tb:
            await self.rollback()
        else:
            await self.commit()

    async def start(self):
        func = self.conn.manual_commit
        if inspect.iscoroutinefunction(func):
            await func()
        else:
            func()

    async def commit(self):
        await self.conn.raw_conn().commit()

    async def rollback(self):
        await self.conn.raw_conn().rollback()
