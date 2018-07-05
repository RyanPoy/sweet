#coding: utf8
from sweet.tests.unit import TestCase
from sweet.query import MysqlSQLBuilder


class MysqlSQLBuilderTest(TestCase):

    def get_builder(self):
        return MysqlSQLBuilder()

    def test_basic_select(self):
        sb = self.get_builder()
        sb.select('*').from_('users')
        self.assertEqual('SELECT * FROM "users"', sb.to_sql())

    def test_multi_select(self):
        sb = self.get_builder()
        sb.select('id', 'name').from_('users')
        self.assertEqual('SELECT "id", "name" FROM "users"', sb.to_sql())

    def test_select_twice(self):
        sb = self.get_builder()
        sb.select('id').from_('users').select('name')
        self.assertEqual('SELECT "id", "name" FROM "users"', sb.to_sql())


if __name__ == '__main__':
    import unittest
    unitest.testmain()

    