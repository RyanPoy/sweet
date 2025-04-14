import pytest


@pytest.mark.asyncio
async def test_insert_and_select(pg_env):
    """测试插入和查询功能."""
    count = await pg_env.db.execute_rowcount("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_1")
    assert count == 1

    results = await pg_env.db.fetchone("SELECT id, name FROM \"users\"")
    assert results == {'id': 1, 'name': 'test_name_1'}


@pytest.mark.asyncio
async def test_transaction_commit(pg_env):
    """测试事务提交."""
    async with pg_env.db.transaction():
        await pg_env.db.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_1")
        await pg_env.db.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_2")

    # 查询数据
    results = await pg_env.db.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_rollback(pg_env):
    """测试事务回滚功能."""
    try:
        async with pg_env.db.transaction():
            await pg_env.db.execute("INSERT INTO \"users\" (name) VALUES ($1)", "rollback_test")
            raise Exception("Rollback this transaction")
    except:  # noqa: E722
        pass  # 模拟回滚

    result = await pg_env.db.fetchone("SELECT * FROM \"users\" WHERE name = $1", "rollback_test")
    assert result is None  # 回滚后数据不应存在


@pytest.mark.asyncio
async def test_select_without_transaction(pg_env):
    """测试非事务情况下的查询. 默认：事务自动提交"""
    select_query = "SELECT COUNT(*) as count FROM \"users\""
    result = await pg_env.db.fetchone(select_query)
    assert result == {"count": 0}
