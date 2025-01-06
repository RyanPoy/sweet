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
        self.assertEqual('FOR UPDATE', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE', lock.sql(self.pg))

    def test_where_lock_share(self):
        lock = Lock(share=True)
        self.assertEqual('FOR UPDATE SHARE', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE SHARE', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE SHARE', lock.sql(self.pg))

    def test_where_lock_nowait(self):
        lock = Lock(nowait=True)
        self.assertEqual('FOR UPDATE NOWAIT', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE NOWAIT', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE NOWAIT', lock.sql(self.pg))

    def test_where_lock_skip(self):
        lock = Lock(skip=True)
        self.assertEqual('FOR UPDATE SKIP LOCKED', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE SKIP LOCKED', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE SKIP LOCKED', lock.sql(self.pg))

    def test_where_lock_of(self):
        lock = Lock(of=("abc",))
        self.assertEqual('FOR UPDATE OF `abc`', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE OF "abc"', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE OF "abc"', lock.sql(self.pg))

    def test_where_lock_skip_locked_and_of(self):
        lock = Lock(skip=True, of=("abc",))
        self.assertEqual('FOR UPDATE OF `abc` SKIP LOCKED', lock.sql(self.mysql))
        self.assertEqual('FOR UPDATE OF "abc" SKIP LOCKED', lock.sql(self.sqlite))
        self.assertEqual('FOR UPDATE OF "abc" SKIP LOCKED', lock.sql(self.pg))


if __name__ == '__main__':
    unittest.main()
