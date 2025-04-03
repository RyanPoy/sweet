import pytest


class TestInsertAndSelect:
    """测试插入和查询功能."""

    @pytest.mark.asyncio
    async def test_mysql(self, mysql_env):
        await mysql_env.db.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_1", 1001)
        results = await mysql_env.db.fetchone("SELECT id, name, code FROM `foos`")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}

    @pytest.mark.asyncio
    async def test_sqlite(self, sqlite_env):
        await sqlite_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
        results = await sqlite_env.db.fetchone("SELECT id, name, code FROM \"foos\"")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}

    @pytest.mark.asyncio
    async def test_pg(self, pg_env):
        await pg_env.db.execute_rowcount("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_1", 1001)
        results = await pg_env.db.fetchone("SELECT id, name, code FROM \"foos\"")
        assert results == {'id': 1, 'name': 'test_name_1', 'code': 1001}


class TestTransactionCommit:
    """测试事务提交."""

    @pytest.mark.asyncio
    async def test_mysql(self, mysql_env):
        async with mysql_env.db.transaction():
            await mysql_env.db.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_1", 1001)
            await mysql_env.db.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "test_name_2", 1002)

        # 查询数据
        results = await mysql_env.db.fetchall("SELECT id, name, code FROM `foos`")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}

    @pytest.mark.asyncio
    async def test_pg(self, pg_env):
        async with pg_env.db.transaction():
            await pg_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_1", 1001)
            await pg_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "test_name_2", 1002)

        # 查询数据
        results = await pg_env.db.fetchall("SELECT id, name, code FROM \"foos\"")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}

    @pytest.mark.asyncio
    async def test_sqlite(self, sqlite_env):
        async with sqlite_env.db.transaction():
            await sqlite_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_1", 1001)
            await sqlite_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "test_name_2", 1002)

        # 查询数据
        results = await sqlite_env.db.fetchall("SELECT id, name, code FROM \"foos\"")
        assert len(results) == 2
        assert results[0] == {'id': 1, 'name': 'test_name_1', 'code': 1001}
        assert results[1] == {'id': 2, 'name': 'test_name_2', 'code': 1002}


class TestTransactionRollback:
    """测试事务回滚功能."""

    @pytest.mark.asyncio
    async def test_mysql(self, mysql_env):
        try:
            async with mysql_env.db.transaction():
                await mysql_env.db.execute("INSERT INTO `foos` (name, code) VALUES (%s, %s)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await mysql_env.db.fetchone("SELECT id FROM `foos` WHERE name = %s", "rollback_test")
        assert result is None  # 回滚后数据不应存在

    @pytest.mark.asyncio
    async def test_sqlite(self, sqlite_env):
        try:
            async with sqlite_env.db.transaction():
                await sqlite_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES (?, ?)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await sqlite_env.db.fetchone("SELECT id FROM \"foos\" WHERE name = ?", "rollback_test")
        assert result is None  # 回滚后数据不应存在

    @pytest.mark.asyncio
    async def test_pg(self, pg_env):
        try:
            async with pg_env.db.transaction():
                await pg_env.db.execute("INSERT INTO \"foos\" (name, code) VALUES ($1, $2)", "rollback_test", 3001)
                raise Exception("Rollback this transaction")
        except:  # noqa: E722
            pass  # 模拟回滚

        result = await pg_env.db.fetchone("SELECT * FROM \"foos\" WHERE name = $1", "rollback_test")
        assert result is None  # 回滚后数据不应存在


class TestSelectWithoutTransaction:
    """测试非事务情况下的查询. 默认：事务自动提交
    """

    @pytest.mark.asyncio
    async def test_mysql(self, mysql_env):
        select_query = "SELECT COUNT(*) as count FROM `foos`"
        result = await mysql_env.db.fetchone(select_query)
        assert result == {"count": 0}

    @pytest.mark.asyncio
    async def test_sqlite(self, sqlite_env):
        select_query = "SELECT COUNT(*) as count FROM \"foos\""
        result = await sqlite_env.db.fetchone(select_query)
        assert result == {"count": 0}

    @pytest.mark.asyncio
    async def test_pg(self, pg_env):
        select_query = "SELECT COUNT(*) as count FROM \"foos\""
        result = await pg_env.db.fetchone(select_query)
        assert result == {"count": 0}
