import pytest


@pytest.mark.asyncio
async def test_mysql_driver__execute_rowids(mysql_env):
    conn = await mysql_env.db.get_connection()
    await conn.execute_rowids("INSERT INTO `users` (name) VALUES (%s)",
                              [("test_name_1",), ("test_name_2",), ("test_name_3",)])

    await conn.execute_rowids("INSERT INTO `users` (id, name) VALUES (%s, %s)",
                              [(12, "test_name_12"), (10, "test_name_10"), (11, "test_name_11")])

    results = await conn.fetchall("SELECT id, name FROM `users` ORDER BY id ASC")
    assert len(results) == 6
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}
    assert results[3] == {'id': 10, 'name': 'test_name_10'}
    assert results[4] == {'id': 11, 'name': 'test_name_11'}
    assert results[5] == {'id': 12, 'name': 'test_name_12'}


@pytest.mark.asyncio
async def test_sqlite_driver__execute_rowids(sqlite_env):
    conn = await sqlite_env.db.get_connection()
    ids = await conn.execute_rowids("INSERT INTO \"users\" (name) VALUES (?) RETURNING rowid",
                                    [("test_name_1",), ("test_name_2",), ("test_name_3",)])
    assert ids == [1, 2, 3]

    ids = await conn.execute_rowids("INSERT INTO \"users\" (id, name) VALUES (?, ?)",
                                    [(12, "test_name_12"), (10, "test_name_10"), (11, "test_name_11")])
    assert ids == [12, 10, 11]

    results = await conn.fetchall("SELECT id, name FROM \"users\" ORDER BY id ASC")
    assert len(results) == 6
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}
    assert results[3] == {'id': 10, 'name': 'test_name_10'}
    assert results[4] == {'id': 11, 'name': 'test_name_11'}
    assert results[5] == {'id': 12, 'name': 'test_name_12'}


@pytest.mark.asyncio
async def test_pg_driver__execute_rowids(pg_env):
    conn = await pg_env.db.get_connection()
    ids = await conn.execute_rowids("INSERT INTO \"users\" (name) VALUES ($1) RETURNING id", ["test_name_1"])
    assert ids == [1]

    ids = await conn.execute_rowids("INSERT INTO \"users\" (name) VALUES ($1) RETURNING id",
                                    [("test_name_2",), ("test_name_3",), ("test_name_4",)])
    assert ids == [2, 3, 4]

    ids = await conn.execute_rowids("INSERT INTO \"users\" (id, name) VALUES ($1, $2) RETURNING id",
                                    [(11, "test_name_11"), (12, "test_name_12"), (10, "test_name_10")])
    assert ids == [11, 12, 10]

    results = await conn.fetchall("SELECT id, name FROM \"users\" ORDER BY id ASC")
    assert len(results) == 7
    assert results[0] == {'id': 1, 'name': 'test_name_1'}
    assert results[1] == {'id': 2, 'name': 'test_name_2'}
    assert results[2] == {'id': 3, 'name': 'test_name_3'}
    assert results[3] == {'id': 4, 'name': 'test_name_4'}
    assert results[4] == {'id': 10, 'name': 'test_name_10'}
    assert results[5] == {'id': 11, 'name': 'test_name_11'}
    assert results[6] == {'id': 12, 'name': 'test_name_12'}
