from abc import ABC, abstractmethod


class AbsTransaction(ABC):
    @abstractmethod
    async def __aenter__(self): pass

    @abstractmethod
    async def __aexit__(self, exc_type, exc_val, exc_tb): pass


class Transaction(AbsTransaction):

    def __init__(self, tx_ctx):
        self._ctx = tx_ctx

    async def __aenter__(self):
        return await self._ctx.__aenter__()

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        return await self._ctx.__aexit__(exc_type, exc_val, exc_tb)
