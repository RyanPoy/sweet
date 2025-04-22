import pytest


@pytest.mark.asyncio
async def test_mysql_driver__execute_rowcount(mysql_env):
    conn = await mysql_env.db.get_connection()

    count = await conn.execute_rowcount("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
    assert count == 1

    count = await conn.execute_rowcount("INSERT INTO `users` (name) VALUES (%s), (%s)", "test_name_2", "test_name_3")
    assert count == 2

    results = await conn.fetchall("SELECT id, name FROM `users`")
    assert len(results) == 3
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}


@pytest.mark.asyncio
async def test_sqlite_driver__execute_rowcount(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
    assert count == 1

    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES (?), (?)", "test_name_2", "test_name_3")
    assert count == 2

    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 3
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}


@pytest.mark.asyncio
async def test_pg_driver_exe_cute_rowcount(pg_env):
    conn = await pg_env.db.get_connection()
    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES ($1)", "test_name_1")
    assert count == 1

    count = await conn.execute_rowcount("INSERT INTO \"users\" (name) VALUES ($1), ($2)", "test_name_2", "test_name_3")
    assert count == 2

    results = await conn.fetchall("SELECT id, name FROM \"users\"")
    assert len(results) == 3
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}
