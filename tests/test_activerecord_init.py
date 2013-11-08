#coding: utf8
from pyrails.record import ActiveRecord
import unittest


class ActiveRecordInitTest(unittest.TestCase):

    def test_init(self):
        class User(ActiveRecord): pass
        u = User(username="abc", password="123")
        self.assertEqual('abc', u.username)
        self.assertEqual('123', u.password)


if __name__ == '__main__':
    unittest.main()
