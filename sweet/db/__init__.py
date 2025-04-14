from typing import Type

from sweet.db.pools import MySQLPool, Pool, PostgreSQLPool, SQLitePool


class DriverFactory:
    @staticmethod
    async def create(config: dict):
        db_type = config["type"]
        if db_type == "pg":
            import asyncpg
            pool = await asyncpg.create_pool(**config["params"])
            return PgDriver(pool)
        elif db_type == "mysql":
            import aiomysql
            pool = await aiomysql.create_pool(**config["params"])
            return MySQLDriver(pool)
        elif db_type == "sqlite":
            return SQLiteDriver(config["params"]["path"])
        else:
            raise ValueError(f"Unsupported db type: {db_type}")
