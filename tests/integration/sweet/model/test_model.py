import unittest

from sweet.environment import Environment
from tests.helper import User, db, settings_mysql, settings_postgresql, settings_sqlite


class ModelTest(unittest.TestCase):

    def setUp(self):
        self.mysql_env = Environment(settings_mysql)
        self.sqlite_env = Environment(settings_sqlite)
        self.pg_env = Environment(settings_postgresql)
        self.envs = (self.mysql_env, self.sqlite_env, self.pg_env)

    def test_insert_and_find(self):
        for env in self.envs:
            async with db.using(env):
                User.objects.create(User(id=1, name="Lily"))
                User.objects.create(User(name="Luly"))
                User.objects.create(User(id=3, name="Jim"))

        for env in self.envs:
            async with db.using(env):
                us = User.objects.all()
                self.assertEqual(3, len(us))
                self.assertEqual()