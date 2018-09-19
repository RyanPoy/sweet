#coding: utf8
from sweet.tests.unit import TestCase
from sweet.query.mysql_sqlbuilder import MysqlSQLBuilder
from sweet.db.conn.mysql_conn import MySQLConn


class MySQLConnTest(TestCase):
    
    def setUp(self):
        self.db = MySQLConn('sweet_test')

    def test_table(self):
        sqlbuilder = self.db.table('users')
        self.assertTrue(isinstance(sqlbuilder, MysqlSQLBuilder))


if __name__ == '__main__':
    import unittest
    unitest.testmain()
