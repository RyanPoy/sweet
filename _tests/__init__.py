#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import unittest
from sweet.db import DBManager
from sweet.orm.model import Model
from sweet.orm.relations import *


class TestCase(unittest.TestCase):
    pass


class UserForTemplateTest(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age


db_mgr = DBManager({
    'default': {
        'driver': 'mysql',
        'host'  : 'localhost',
        'port'  : 3306,
        'dbname': 'sweet_test',
        'user'  : 'root',
        'password': '',
        'show_sql': True,
    }
})
Model.db = db_mgr.new_db('default')

class Foo(Model):
    pass


class User(Model):
    has_many('sweet._tests.Mobile', cascade=True)
    has_one('sweet._tests.Car', cascade=True)


class Mobile(Model):
    belongs_to(User)


class Car(Model):
    belongs_to(User, name='user')


class Article(Model):
    has_and_belongs_to_many('sweet._tests.Tag')


class Tag(Model):
    has_and_belongs_to_many(Article)


class Category(Model):
    has_many('sweet._tests.Category', name='children', fk='parent_id')
    belongs_to('sweet._tests.Category', name='parent', fk='parent_id')


class Score(Model):
    belongs_to('sweet._tests.Student')
    belongs_to('sweet._tests.Course')


class Student(Model):
    has_many(Score)
    has_many('sweet._tests.Course', through=Score)


class Course(Model):
    has_many(Score)
    has_many(Student, through=Score)


class StudentForHasOneThrough(Model):
    __tablename__ = 'students'
    has_one('sweet._tests.ScoreForHasOneThrough', name='score', fk='student_id')
    has_one('sweet._tests.CourseForHasOneThrough', name="course", through="sweet._tests.ScoreForHasOneThrough", through_fk_on_owner='student_id', through_fk_on_target='course_id')


class CourseForHasOneThrough(Model):
    __tablename__ = 'courses'
    has_one('sweet._tests.ScoreForHasOneThrough', name='score', fk='course_id')
    has_one(StudentForHasOneThrough, name="student", through="sweet._tests.ScoreForHasOneThrough", through_fk_on_owner='course_id', through_fk_on_target='student_id')


class ScoreForHasOneThrough(Model):
    __tablename__ = 'scores'
    belongs_to(StudentForHasOneThrough, name='student', fk='student_id')
    belongs_to(CourseForHasOneThrough, name='course', fk='course_id')
