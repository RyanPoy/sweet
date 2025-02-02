import unittest

from sweet.sequel.terms.lock import Lock
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestLock(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_where_lock(self):
        lock = Lock()
        self.assertEqual('FOR UPDATE', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE', self.pg.sql(lock))

    def test_where_lock_share(self):
        lock = Lock(share=True)
        self.assertEqual('FOR UPDATE SHARE', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE SHARE', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE SHARE', self.pg.sql(lock))

    def test_where_lock_nowait(self):
        lock = Lock(nowait=True)
        self.assertEqual('FOR UPDATE NOWAIT', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE NOWAIT', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE NOWAIT', self.pg.sql(lock))

    def test_where_lock_skip(self):
        lock = Lock(skip=True)
        self.assertEqual('FOR UPDATE SKIP LOCKED', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE SKIP LOCKED', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE SKIP LOCKED', self.pg.sql(lock))

    def test_where_lock_of(self):
        lock = Lock(of=("abc",))
        self.assertEqual('FOR UPDATE OF `abc`', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE OF "abc"', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE OF "abc"', self.pg.sql(lock))

    def test_where_lock_skip_locked_and_of(self):
        lock = Lock(skip=True, of=("abc",))
        self.assertEqual('FOR UPDATE OF `abc` SKIP LOCKED', self.mysql.sql(lock))
        self.assertEqual('FOR UPDATE OF "abc" SKIP LOCKED', self.sqlite.sql(lock))
        self.assertEqual('FOR UPDATE OF "abc" SKIP LOCKED', self.pg.sql(lock))


if __name__ == '__main__':
    unittest.main()
