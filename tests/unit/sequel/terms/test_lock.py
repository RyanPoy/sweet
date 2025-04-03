from sweet.sequel.terms.lock import Lock


def test_where_lock(visitors):
    lock = Lock()
    assert visitors.mysql.sql(lock) == 'FOR UPDATE'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE'
    assert visitors.pg.sql(lock) == 'FOR UPDATE'


def test_where_lock_share(visitors):
    lock = Lock(share=True)
    assert visitors.mysql.sql(lock) == 'FOR UPDATE SHARE'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE SHARE'
    assert visitors.pg.sql(lock) == 'FOR UPDATE SHARE'


def test_where_lock_nowait(visitors):
    lock = Lock(nowait=True)
    assert visitors.mysql.sql(lock) == 'FOR UPDATE NOWAIT'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE NOWAIT'
    assert visitors.pg.sql(lock) == 'FOR UPDATE NOWAIT'


def test_where_lock_skip(visitors):
    lock = Lock(skip=True)
    assert visitors.mysql.sql(lock) == 'FOR UPDATE SKIP LOCKED'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE SKIP LOCKED'
    assert visitors.pg.sql(lock) == 'FOR UPDATE SKIP LOCKED'


def test_where_lock_of(visitors):
    lock = Lock(of=("abc",))
    assert visitors.mysql.sql(lock) == 'FOR UPDATE OF `abc`'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE OF "abc"'
    assert visitors.pg.sql(lock) == 'FOR UPDATE OF "abc"'


def test_where_lock_skip_locked_and_of(visitors):
    lock = Lock(skip=True, of=("abc",))
    assert visitors.mysql.sql(lock) == 'FOR UPDATE OF `abc` SKIP LOCKED'
    assert visitors.sqlite.sql(lock) == 'FOR UPDATE OF "abc" SKIP LOCKED'
    assert visitors.pg.sql(lock) == 'FOR UPDATE OF "abc" SKIP LOCKED'
