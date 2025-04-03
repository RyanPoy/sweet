import pytest
import pytest_asyncio

from tests.conftest import ObjDict
from tests.helper.db import DB_TYPE, init_db


@pytest_asyncio.fixture
async def drivers():
    async def init(db_type: DB_TYPE, sql):
        driver = await init_db(db_type)
        await driver.execute(sql)
        return driver

    async def close(drivers):
        for _, driver in drivers.items():
            await driver.execute("""DROP TABLE IF EXISTS foos;""")
            await driver.close_pool()

    ds = ObjDict(
        mysql=await init(DB_TYPE.mysql, """
            CREATE TABLE IF NOT EXISTS `foos` (
                id INT PRIMARY KEY AUTO_INCREMENT,
                name VARCHAR(255) NOT NULL,
                code INT NOT NULL
            );
        """),
        sqlite=await init(DB_TYPE.sqlite, """
            CREATE TABLE IF NOT EXISTS \"foos\" (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name VARCHAR(255) NOT NULL,
                code INTEGER NOT NULL
            );
        """),
        pg=await init(DB_TYPE.pg, """
            CREATE TABLE IF NOT EXISTS "foos" (
                id SERIAL PRIMARY KEY,
                name VARCHAR(255) NOT NULL,
                code INT NOT NULL
            );
        """)
    )
    yield ds

    await close(ds)


class TestInsertAndSelect:
    @pytest.mark.asyncio
    async def test_mysql(self, drivers):
        """测试插入和查询功能."""
        # mysql
        await drivers.mysql.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_1", 1001)
        results = await drivers.mysql.fetchone("SELECT id, name, code FROM `foos`")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}

    @pytest.mark.asyncio
    async def test_sqlite(self, drivers):
        await drivers.sqlite.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
        results = await drivers.sqlite.fetchone("SELECT id, name, code FROM \"foos\"")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}

    @pytest.mark.asyncio
    async def test_pg(self, drivers):
        await drivers.pg.execute_rowcount("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_1", 1001)
        results = await drivers.pg.fetchone("SELECT id, name, code FROM \"foos\"")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}


class TestTransactionCommit:

    @pytest.mark.asyncio
    async def test_mysql(self, drivers):
        """测试插入和查询功能."""
        async with drivers.mysql.transaction():
            await drivers.mysql.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_1", 1001)
            await drivers.mysql.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_2", 1002)

        # 查询数据
        results = await drivers.mysql.fetchall("SELECT id, name, code FROM `foos`")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}

    @pytest.mark.asyncio
    async def test_sqlite(self, drivers):
        """测试插入和查询功能."""
        async with drivers.sqlite.transaction():
            await drivers.sqlite.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
            await drivers.sqlite.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_2", 1002)

        # 查询数据
        results = await drivers.sqlite.fetchall("SELECT id, name, code FROM \"foos\"")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}

    @pytest.mark.asyncio
    async def test_pg(self, drivers):
        """测试插入和查询功能."""
        async with drivers.pg.transaction():
            await drivers.pg.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_1", 1001)
            await drivers.pg.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_2", 1002)

        # 查询数据
        results = await drivers.pg.fetchall("SELECT id, name, code FROM \"foos\"")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}


class TestTransactionRollback:
    @pytest.mark.asyncio
    async def test_mysql(self, drivers):
        """测试事务回滚功能."""
        try:
            async with drivers.mysql.transaction():
                await drivers.mysql.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await drivers.mysql.fetchone("SELECT id FROM `foos` WHERE name = %s", "rollback_test")
        assert result is None  # 回滚后数据不应存在

    @pytest.mark.asyncio
    async def test_sqlite(self, drivers):
        """测试事务回滚功能."""
        try:
            async with drivers.sqlite.transaction():
                await drivers.sqlite.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await drivers.sqlite.fetchone("SELECT id FROM \"foos\" WHERE name = ?", "rollback_test")
        assert result is None  # 回滚后数据不应存在

    @pytest.mark.asyncio
    async def test_pg(self, drivers):
        """测试事务回滚功能."""
        try:
            async with drivers.pg.transaction():
                await drivers.pg.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await drivers.pg.fetchone("SELECT * FROM \"foos\" WHERE name = $1", "rollback_test")
        assert result is None  # 回滚后数据不应存在


class TestSelectWithoutTransaction:

    @pytest.mark.asyncio
    async def test_mysql(self, drivers):
        """测试非事务情况下的查询."""
        select_query = "SELECT COUNT(*) as count FROM `foos`"
        result = await drivers.mysql.fetchone(select_query)
        assert result == {"count": 0}

    @pytest.mark.asyncio
    async def test_sqlite(self, drivers):
        """测试非事务情况下的查询."""
        select_query = "SELECT COUNT(*) as count FROM \"foos\""
        result = await drivers.sqlite.fetchone(select_query)
        assert result == {"count": 0}

    @pytest.mark.asyncio
    async def test_pg(self, drivers):
        """测试非事务情况下的查询."""
        select_query = "SELECT COUNT(*) as count FROM \"foos\""
        result = await drivers.pg.fetchone(select_query)
        assert result == {"count": 0}
