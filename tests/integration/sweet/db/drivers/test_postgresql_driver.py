import pytest


@pytest.mark.asyncio
async def test_transaction_commit_manual(pg_env):
    """测试事务提交."""
    conn = await pg_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    await conn.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_1")
    await conn.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_2")
    await tran.commit()

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_commit_use_with(pg_env):
    """测试事务提交."""
    conn = await pg_env.db.get_connection()
    async with conn.transaction():
        await conn.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_1")
        await conn.execute("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_2")

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_rollback_manual(pg_env):
    """测试事务回滚功能."""
    conn = await pg_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES ($1)", "rollback_test")
    assert count == 1
    await tran.rollback()

    result = await conn.fetchone("SELECT id FROM \"users\" WHERE name = ($1)", "rollback_test")
    assert result is None  # 回滚后数据不应存在


@pytest.mark.asyncio
async def test_transaction_rollback(pg_env):
    """测试事务回滚功能."""
    conn = await pg_env.db.get_connection()
    try:
        async with conn.transaction():
            count = await conn.execute("INSERT INTO \"users\" (name) VALUES ($1)", "rollback_test")
            assert count == 1
            raise Exception("Rollback this transaction")
    except:  # noqa: E722
        pass  # 模拟回滚

    result = await conn.fetchone("SELECT * FROM \"users\" WHERE name = $1", "rollback_test")
    assert result is None  # 回滚后数据不应存在
