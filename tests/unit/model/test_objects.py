import unittest

from sweet.model import Model
from sweet.model.objects import Objects
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor
from tests.helper import User


class TestObjects(unittest.TestCase):
    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_init_from_model(self):
        class Demo(Model): pass

        objs = Demo.objects
        self.assertIsInstance(objs, Objects)
        self.assertIs(objs.model_class, Demo)

    def test_filter(self):
        objs1 = User.objects.filter(id=10)
        objs2 = objs1.filter(name="username")
        self.assertNotEqual(objs1.binary, objs2.binary)

    def test_all(self):
        users = User.objects.filter(id=10).filter(name="username").all()
        self.assertNotEqual(users, 1)


if __name__ == '__main__':
    unittest.main()
