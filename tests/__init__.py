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
    has_many('sweet.tests.Mobile', cascade=True)
    has_one('sweet.tests.Car', cascade=True)


class Mobile(Model):
    belongs_to(User)


class Car(Model):
    belongs_to(User, name='user')


class Article(Model):
    has_and_belongs_to_many('sweet.tests.Tag')


class Tag(Model):
    has_and_belongs_to_many(Article)


class Category(Model):
    has_many('sweet.tests.Category', name='children', fk='parent_id')
    belongs_to('sweet.tests.Category', name='parent', fk='parent_id')


class Score(Model):
    belongs_to('sweet.tests.Student')
    belongs_to('sweet.tests.Course')


class Student(Model):
    has_many(Score)
    has_many('sweet.tests.Course', through=Score)


class Course(Model):
    has_many(Score)
    has_many(Student, through=Score)


class StudentForHasOneThrough(Model):
    __tablename__ = 'students'
    has_one('sweet.tests.ScoreForHasOneThrough', name='score', fk='student_id')
    has_one('sweet.tests.CourseForHasOneThrough', name="course", through="sweet.tests.ScoreForHasOneThrough", through_fk_on_owner='student_id', through_fk_on_target='course_id')


class CourseForHasOneThrough(Model):
    __tablename__ = 'courses'
    has_one('sweet.tests.ScoreForHasOneThrough', name='score', fk='course_id')
    has_one(StudentForHasOneThrough, name="student", through="sweet.tests.ScoreForHasOneThrough", through_fk_on_owner='course_id', through_fk_on_target='student_id')


class ScoreForHasOneThrough(Model):
    __tablename__ = 'scores'
    belongs_to(StudentForHasOneThrough, name='student', fk='student_id')
    belongs_to(CourseForHasOneThrough, name='course', fk='course_id')
