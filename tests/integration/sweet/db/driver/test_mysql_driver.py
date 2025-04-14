import pytest


@pytest.mark.asyncio
async def test_insert_and_select(mysql_env):
    """测试插入和查询功能."""
    count = await mysql_env.db.execute_rowcount("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
    assert count == 1

    results = await mysql_env.db.fetchone("SELECT id, name FROM `users`")
    assert results == {'id': 1, 'name': 'test_name_1'}


@pytest.mark.asyncio
async def test_transaction_commit(mysql_env):
    """测试事务提交."""
    async with mysql_env.db.transaction():
        await mysql_env.db.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
        await mysql_env.db.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_2")

    # 查询数据
    results = await mysql_env.db.fetchall("SELECT id, name FROM `users`")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_transaction_rollback(mysql_env):
    """测试事务回滚功能."""
    try:
        async with mysql_env.db.transaction():
            count = await mysql_env.db.execute_rowcount("INSERT INTO `users` (name) VALUES (%s)", "rollback_test")
            assert count == 1
            raise Exception("Rollback this transaction")
    except:  # noqa: E722
        pass  # 模拟回滚

    result = await mysql_env.db.fetchone("SELECT id FROM `users` WHERE name = %s", "rollback_test")
    assert result is None  # 回滚后数据不应存在


@pytest.mark.asyncio
async def test_select_without_transaction(mysql_env):
    """测试非事务情况下的查询. 默认：事务自动提交"""
    select_query = "SELECT COUNT(*) as count FROM `users`"
    result = await mysql_env.db.fetchone(select_query)
    assert result == {"count": 0}
