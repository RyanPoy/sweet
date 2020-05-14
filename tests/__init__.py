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
    belongs_to(User)


class Car(Model):
    belongs_to(User, name='user')


class Article(Model):
    has_and_belongs_to_many('tags', 'sweet.tests.Tag')


class Tag(Model):
    has_and_belongs_to_many('articles', Article)


class Category(Model):
    has_many('children', 'sweet.tests.Category', fk='parent_id')
    belongs_to('sweet.tests.Category', name='parent', fk='parent_id')


class Student(Model):
    has_many('scores', 'sweet.tests.Score')
    has_many('courses', 'sweet.tests.Course', through="sweet.tests.Score")


class Course(Model):
    has_many('scores', 'sweet.tests.Score')
    has_many('students', 'sweet.tests.Student', through="sweet.tests.Score")


class Score(Model):
    belongs_to(Student, name='student')
    belongs_to(Course, name='course')


class StudentForHasOneThrough(Model):
    __tablename__ = 'students'
    has_one('score', 'sweet.tests.Score')
    has_one('course', 'sweet.tests.Course', through="sweet.tests.Score", through_fk_on_owner='student_id', through_fk_on_target='course_id')


class CourseForHasOneThrough(Model):
    __tablename__ = 'courses'
    has_one('score', 'sweet.tests.Score')
    has_one('student', 'sweet.tests.Student', through="sweet.tests.Score", through_fk_on_owner='course_id', through_fk_on_target='student_id')


class ScoreForHasOneThrough(Model):
    __tablename__ = 'scores'
    belongs_to(StudentForHasOneThrough, name='student', fk='student_id')
    belongs_to(CourseForHasOneThrough, name='course', fk='course_id')
