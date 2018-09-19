#coding: utf8
from sweet.tests.unit import TestCase
from sweet.database.table import MySQLTable
from sweet.database import MySQL


class MySQLTest(TestCase):
    
    def setUp(self):
        self.db = MySQL('sweet_test')

    def test_table(self):
        sqlbuilder = self.db.table('users')
        self.assertTrue(isinstance(sqlbuilder, MySQLTable))


if __name__ == '__main__':
    import unittest
    unitest.testmain()
