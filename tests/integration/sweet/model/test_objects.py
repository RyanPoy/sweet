import unittest

from sweet.environment import Environment
from sweet.model import Column, Model
from sweet.model.objects import Objects
from tests import helper
from tests.helper import db, settings_mysql, settings_postgresql, settings_sqlite, User
from tests.helper.sqls import mysql_sql, postgres_sql, sqlite_sql


class TestObjects(unittest.IsolatedAsyncioTestCase):

    async def asyncSetUp(self):
        self.mysql_env = Environment(settings_mysql)
        self.sqlite_env = Environment(settings_sqlite)
        self.pg_env = Environment(settings_postgresql)
        self.envs = (self.mysql_env, self.sqlite_env, self.pg_env)

        for env, sqls in zip(self.envs, [mysql_sql.CREATE_SQLS, sqlite_sql.CREATE_SQLS, postgres_sql.CREATE_SQLS]):
            async with db.using(env) as driver:
                await helper.create_tables_for_model(driver, sqls)

    async def asyncTearDown(self):
        for env, sqls in zip(self.envs, [mysql_sql.DROP_SQLS, sqlite_sql.DROP_SQLS, postgres_sql.DROP_SQLS]):
            async with db.using(env) as driver:
                await helper.delete_all_models(driver, sqls)

    def test_init_from_model(self):
        class Demo(Model): pass

        objs = Demo.objects
        self.assertIsInstance(objs, Objects)
        self.assertIs(objs.model_class, Demo)

    async def test_filter(self):
        objs1 = User.objects.filter(id=10)
        objs2 = objs1.filter(name="username")
        self.assertNotEqual(objs1._select_stmt, objs2._select_stmt)
        for i, env in enumerate(self.envs):
            async with db.using(env):
                self.assertNotEqual(objs1.sql(), objs2.sql())

    async def test_filter__error_if_column_is_not_exists(self):
        with self.assertRaises(Column.DoesNotExist) as ex:
            User.objects.filter(not_exist_column_name="whatever")
        msg = str(ex.exception)
        self.assertEqual(f"The column 'not_exist_column_name' does not exist in '{User.table.name}' table", msg)

    async def test_all(self):
        objs = User.objects.filter(id=10).filter(name="username")
        expectations = [
            """SELECT * FROM `users` WHERE `id` = 10 AND `name` = 'username'""",
            """SELECT * FROM "users" WHERE "id" = 10 AND "name" = 'username'""",
            """SELECT * FROM "users" WHERE "id" = 10 AND "name" = 'username'""",
        ]
        for i, env in enumerate(self.envs):
            expected = expectations[i]
            async with db.using(env) as driver:
                sql = objs.sql()
                self.assertEqual(expected, sql, f'Environment[{driver.__class__.__name__}]')

    async def test_insert_and_first(self):
        for i, env in enumerate(self.envs):
            async with db.using(env):
                u1 = await User.objects.insert({'id': 1, 'name': "lily"})
                u2 = await User.objects.first()
                self.assertEqual(u2.id, 1)
                self.assertEqual(u2.name, "lily")


if __name__ == '__main__':
    unittest.main()
