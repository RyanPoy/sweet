import unittest

from tests.helper.db import DB_TYPE, close, init_db


class TestSQLiteDriver(unittest.IsolatedAsyncioTestCase):

    async def asyncSetUp(self):
        self.driver = await init_db(DB_TYPE.sqlite)
        await self.driver.execute("""
            CREATE TABLE IF NOT EXISTS \"foos\" (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name VARCHAR(255) NOT NULL,
                code INTEGER NOT NULL
            );
        """)

    async def asyncTearDown(self):
        await self.driver.execute("""DROP TABLE IF EXISTS \"foos\";""")
        await close(self.driver)

    async def test_insert_and_select(self):
        """测试插入和查询功能."""
        await self.driver.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
        results = await self.driver.fetchone("SELECT id, name, code FROM \"foos\"")
        self.assertEqual((1, 'test_name_1', 1001), results)

    async def test_transaction_commit(self):
        """测试插入和查询功能."""
        async with self.driver.transaction():
            await self.driver.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
            await self.driver.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_2", 1002)

        # 查询数据
        results = await self.driver.fetchall("SELECT id, name, code FROM \"foos\"")
        self.assertEqual(2, len(results))
        self.assertEqual((1, 'test_name_1', 1001), results[0])
        self.assertEqual((2, 'test_name_2', 1002), results[1])

    async def test_transaction_rollback(self):
        """测试事务回滚功能."""
        try:
            async with self.driver.transaction():
                await self.driver.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:
            pass  # 模拟回滚

        result = await self.driver.fetchone("SELECT id FROM \"foos\" WHERE name = ?", "rollback_test")
        self.assertIsNone(result)  # 回滚后数据不应存在

    async def test_select_without_transaction(self):
        """测试非事务情况下的查询."""
        select_query = "SELECT COUNT(*) as count FROM \"foos\""
        result = await self.driver.fetchone(select_query)
        self.assertEqual((0,), result)


if __name__ == "__main__":
    unittest.main()

