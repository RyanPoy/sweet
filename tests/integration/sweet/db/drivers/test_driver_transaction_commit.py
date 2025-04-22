import pytest


@pytest.mark.asyncio
async def test_mysql_transaction_commit_manual(mysql_env):
    """测试事务提交."""
    conn = await mysql_env.db.get_connection()
    tran = conn.transaction()
    await tran.start()
    await conn.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
    await conn.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_2")
    await tran.commit()

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM `users`")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_mysql_transaction_commit_use_with(mysql_env):
    """测试事务提交."""
    conn = await mysql_env.db.get_connection()
    async with conn.transaction():
        await conn.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
        await conn.execute("INSERT INTO `users` (name) VALUES (%s)", "test_name_2")

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM `users`")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_sqlite_transaction_commit_manual(sqlite_env):
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
async def test_sqlite_transaction_commit_use_with(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    async with conn.transaction():
        await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
        await conn.execute("INSERT INTO \"users\" (name) VALUES (?)", "test_name_2")

    # 查询数据
    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 2
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}


@pytest.mark.asyncio
async def test_pg_transaction_commit_manual(pg_env):
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
async def test_pg_transaction_commit_use_with(pg_env):
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
