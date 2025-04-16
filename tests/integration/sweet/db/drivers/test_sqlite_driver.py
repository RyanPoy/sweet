import pytest


@pytest.mark.asyncio
async def test_insert_and_select(sqlite_env):
    count = await sqlite_env.db.execute_rowcount("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
    assert count == 1

    results = await sqlite_env.db.fetchone("SELECT id, name FROM \"users\"")
    assert results == {'id': 1, 'name': 'test_name_1'}


@pytest.mark.asyncio
async def test_transaction_commit(sqlite_env):
    async with sqlite_env.db.transaction():
        await sqlite_env.db.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
        await sqlite_env.db.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_2")

    # 查询数据
    results = await sqlite_env.db.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_rollback(sqlite_env):
    try:
        async with sqlite_env.db.transaction():
            await sqlite_env.db.execute("INSERT INTO \"users\" (name) VALUES (?)", "rollback_test")
            raise Exception("Rollback this transaction")
    except:  # noqa: E722
        pass  # 模拟回滚

    result = await sqlite_env.db.fetchone("SELECT id FROM \"users\" WHERE name = ?", "rollback_test")
    assert result is None  # 回滚后数据不应存在


@pytest.mark.asyncio
async def test_select_without_transaction(sqlite_env):
    select_query = "SELECT COUNT(*) as count FROM \"users\""
    result = await sqlite_env.db.fetchone(select_query)
    assert result == {"count": 0}
