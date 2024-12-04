import unittest

from sweet.sequel.sequel import mysql, sqlite, pg
from sweet.sequel.insert import Insert


class TestInsertQuery(unittest.TestCase):

    def setUp(self):
        table_name = 'users'
        self.users_mysql = Insert(mysql, table_name)
        self.users_sqlite = Insert(sqlite, table_name)
        self.users_pg = Insert(pg, table_name)

    def test_insert_simple(self):
        sql, params = self.users_mysql.insert(username='charlie', superuser=0, admin=1).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s)', sql)
        self.assertEqual(['charlie', 0, 1], params)

        sql, params = self.users_sqlite.insert(username='charlie', superuser=0, admin=1).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?)', sql)
        self.assertEqual(['charlie', 0, 1], params)

        sql, params = self.users_pg.insert(username='charlie', superuser=0, admin=1).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3)', sql)
        self.assertEqual(['charlie', 0, 1], params)

    def test_insert_list(self):
        data = [
            {'username': 'charlie', 'superuser': 0, 'admin': 1},
            {'username': 'lucy', 'superuser': 0, 'admin': 0},
            {'username': 'jim', 'superuser': 1, 'admin': 0},
            {'username': 'mary', 'superuser': 1, 'admin': 1},
        ]
        sql, params = self.users_mysql.insert(data).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s), (%s, %s, %s), (%s, %s, %s), (%s, %s, %s)', sql)
        self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)

        sql, params = self.users_sqlite.insert(data).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?), (?, ?, ?), (?, ?, ?), (?, ?, ?)', sql)
        self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)

        sql, params = self.users_pg.insert(data).sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9), ($10, $11, $12)', sql)
        self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)

    def test_insert_returning(self):
        data = [
            {'username': 'charlie', 'superuser': 0, 'admin': 1},
            {'username': 'mary', 'superuser': 1, 'admin': 1},
        ]
        sql, params = self.users_mysql.insert(data).returning('username', 'admin').sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s), (%s, %s, %s) RETURNING ("username", "admin")', sql)
        self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)

        sql, params = self.users_sqlite.insert(data).returning('username', 'admin').sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?), (?, ?, ?) RETURNING ("username", "admin")', sql)
        self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)

        sql, params = self.users_pg.insert(data).returning('username', 'admin').sql()
        self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3), ($4, $5, $6) RETURNING ("username", "admin")', sql)
        self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)

    # def test_empty(self):
    #     class Empty(TestModel): pass
    #     query = Empty.insert()
    #     if isinstance(db, MySQLDatabase):
    #         sql = 'INSERT INTO "empty" () VALUES ()'
    #     elif isinstance(db, PostgresqlDatabase):
    #         sql = 'INSERT INTO "empty" DEFAULT VALUES RETURNING "empty"."id"'
    #     else:
    #         sql = 'INSERT INTO "empty" DEFAULT VALUES'
    #     self.assertSQL(query, sql, [])

    # @requires_sqlite
    # def test_replace_sqlite(self):
    #     query = User.replace({
    #         User.c.username: 'charlie',
    #         User.c.superuser: False})
    #     self.assertSQL(query, (
    #         'INSERT OR REPLACE INTO "users" ("superuser", "username") '
    #         'VALUES (?, ?)'), [False, 'charlie'])
    #
    # @requires_mysql
    # def test_replace_mysql(self):
    #     query = User.replace({
    #         User.c.username: 'charlie',
    #         User.c.superuser: False})
    #     self.assertSQL(query, (
    #         'REPLACE INTO "users" ("superuser", "username") '
    #         'VALUES (?, ?)'), [False, 'charlie'])


if __name__ == '__main__':
    unittest.main()
