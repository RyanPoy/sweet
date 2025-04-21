import pytest


@pytest.mark.asyncio
async def test_insert_and_select(sqlite_env):
    """测试插入和查询功能."""
    conn = await sqlite_env.db.get_connection()
    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
    assert count == 1

    results = await conn.fetchone("SELECT id, name FROM \"users\"")
    assert results == {'id': 1, 'name': 'test_name_1'}


@pytest.mark.asyncio
async def test_transaction_commit_manual(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
    await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_2")
    await tran.commit()

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_commit_use_with(sqlite_env):
    conn = sqlite_env.db.get_connection()
    async with conn.transaction():
        await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
        await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_2")

    # 查询数据
    results = await sqlite_env.db.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_rollback_manual(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "rollback_test")
    await tran.rollback()

    result = await sqlite_env.db.fetchone("SELECT id FROM \"users\" WHERE name = ?", "rollback_test")
    assert result is None  # 回滚后数据不应存在


@pytest.mark.asyncio
async def test_transaction_rollback_use_with(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    try:
        async with conn.transaction():
            await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "rollback_test")
            raise Exception("Rollback this transaction")
    except:  # noqa: E722
        pass  # 模拟回滚

    result = await conn.fetchone("SELECT id FROM \"users\" WHERE name = ?", "rollback_test")
    assert result is None  # 回滚后数据不应存在
