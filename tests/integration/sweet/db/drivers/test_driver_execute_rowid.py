import pytest


@pytest.mark.asyncio
async def test_mysql_driver__execute_rowid(mysql_env):
    conn = await mysql_env.db.get_connection()

    id = await conn.execute_rowid("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
    assert id == 1

    id = await conn.execute_rowid("INSERT INTO `users` (id, name) VALUES (%s, %s)", 0, "test_name_2")
    assert id == 2

    id = await conn.execute_rowid("INSERT INTO `users` (id, name) VALUES (%s, %s)", 10, "test_name_3")
    assert id == 10

    results = await conn.fetchall("SELECT id, name FROM `users` ORDER BY id ASC")
    assert len(results) == 3
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 10, 'name': 'test_name_3'}

@pytest.mark.asyncio
async def test_mysql_driver__execute_rowid_use_with(mysql_env):
    async with await mysql_env.db.get_connection() as conn:
        id = await conn.execute_rowid("INSERT INTO `users` (name) VALUES (%s)", "test_name_1")
        assert id == 1

        id = await conn.execute_rowid("INSERT INTO `users` (id, name) VALUES (%s, %s)", 0, "test_name_2")
        assert id == 2

        id = await conn.execute_rowid("INSERT INTO `users` (id, name) VALUES (%s, %s)", 10, "test_name_3")
        assert id == 10

        results = await conn.fetchall("SELECT id, name FROM `users` ORDER BY id ASC")
        assert len(results) == 3
        assert results[0] == {'id': 1, 'name': 'test_name_1'}
        assert results[1] == {'id': 2, 'name': 'test_name_2'}
        assert results[2] == {'id': 10, 'name': 'test_name_3'}


@pytest.mark.asyncio
async def test_sqlite_driver__execute_rowid(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    id = await conn.execute_rowid("INSERT INTO \"users\" (name) VALUES (?)", "test_name_1")
    assert id == 1

    id = await conn.execute_rowid("INSERT INTO \"users\" (id, name) VALUES (?, ?)", 0, "test_name_2")
    assert id == 0

    id = await conn.execute_rowid("INSERT INTO \"users\" (id, name) VALUES (?, ?)", 10, "test_name_3")
    assert id == 10

    results = await conn.fetchall("SELECT id, name FROM \"users\" ORDER BY id ASC")
    assert len(results) == 3
    assert results[0] == {'id': 0, 'name': 'test_name_2'}
    assert results[1] == {'id': 1, 'name': 'test_name_1'}
    assert results[2] == {'id': 10, 'name': 'test_name_3'}


@pytest.mark.asyncio
async def test_pg_driver__execute_rowid(pg_env):
    conn = await pg_env.db.get_connection()
    id = await conn.execute_rowid("INSERT INTO \"users\" (name) VALUES ($1) RETURNING id", "test_name_1")
    assert id == 1

    id = await conn.execute_rowid("INSERT INTO \"users\" (id, name) VALUES ($1, $2) RETURNING id", 0, "test_name_2")
    assert id == 0

    id = await conn.execute_rowid("INSERT INTO \"users\" (id, name) VALUES ($1, $2) RETURNING id", 10, "test_name_3")
    assert id == 10

    results = await conn.fetchall("SELECT id, name FROM \"users\" ORDER BY id ASC")
    assert len(results) == 3
    assert results[0] == {'id': 0, 'name': 'test_name_2'}
    assert results[1] == {'id': 1, 'name': 'test_name_1'}
    assert results[2] == {'id': 10, 'name': 'test_name_3'}
