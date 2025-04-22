import pytest



@pytest.mark.asyncio
async def test_transaction_rollback_manual(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "rollback_test")
    await tran.rollback()

    result = await conn.fetchone("SELECT id FROM \"users\" WHERE name = ?", "rollback_test")
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
