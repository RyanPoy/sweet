import unittest

from sweet.environment import Environment
from sweet.model import Model
from sweet.model.objects import Objects
from tests.helper import db, settings_mysql, settings_postgresql, settings_sqlite, User


class TestObjects(unittest.IsolatedAsyncioTestCase):

    def setUp(self):
        self.mysql_env = Environment(settings_mysql)
        self.sqlite_env = Environment(settings_sqlite)
        self.pg_env = Environment(settings_postgresql)
        self.envs = (self.mysql_env, self.sqlite_env, self.pg_env)

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

    # async def test_insert_and_first(self):
    #
    #     expectations = [
    #         """SELECT * FROM `users` LIMIT 1""",
    #         """SELECT * FROM "users" LIMIT 1""",
    #         """SELECT * FROM "users" LIMIT 1""",
    #     ]
    #     user = User(id=1, name=20)
    #     for i, env in enumerate(self.envs):
    #         u = User.objects.insert(user.dict())
    #         async with db.using(env):
    #             u = await User.objects.first()
    #             self.assertEqual(u, '')


if __name__ == '__main__':
    unittest.main()
