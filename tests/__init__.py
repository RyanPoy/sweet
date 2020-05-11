#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import unittest
from sweet.database import DBManager
from sweet.orm.model import Model
from sweet.orm.relations import *


class TestCase(unittest.TestCase):
    pass


class UserForTemplateTest(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age


Model.db_manager = DBManager({
    'driver': 'mysql',
    'host': 'localhost',
    'port': 3306,
    'database': 'sweet_test',
    'user': 'root',
    'password': '',
    'show_sql': True,
})

class Foo(Model):
    pass


class User(Model):
    has_many('mobiles', 'sweet.tests.Mobile', cascade=True)
    has_one('car', 'sweet.tests.Car', cascade=True)


class Mobile(Model):
    belongs_to('user', User)


class Car(Model):
    belongs_to('user', User)


class Article(Model):
    has_and_belongs_to_many('tags', 'sweet.tests.Tag')


class Tag(Model):
    has_and_belongs_to_many('articles', Article)

