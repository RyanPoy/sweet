#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from sweet.record import DBManager
from sweet.record.model import Model
from sweet.record.relations import *

db_mgr = DBManager({
    'default': {
        'drivers': 'mysql',
        'host'  : 'localhost',
        'port'  : 3306,
        'dbname': 'sweet_test',
        'user'  : 'root',
        'password': '',
        'show_sql': True,
    }
})


class AbsModel(Model):

    __abs = True
    db = db_mgr.new_db('default')


class Foo(AbsModel):
    pass


class User(AbsModel):
    has_many('tests.integration.for_mysql.helper.Mobile', cascade=True)
    has_one('tests.integration.for_mysql.helper.Car', cascade=True)


class Mobile(AbsModel):
    belongs_to(User)


class Car(AbsModel):
    belongs_to(User, name='user')


class Article(AbsModel):
    has_and_belongs_to_many('tests.integration.for_mysql.helper.Tag')


class Tag(AbsModel):
    has_and_belongs_to_many(Article)


class Category(AbsModel):
    has_many('tests.integration.for_mysql.helper.Category', name='children', fk='parent_id')
    belongs_to('tests.integration.for_mysql.helper.Category', name='parent', fk='parent_id')


class Score(AbsModel):
    belongs_to('tests.integration.for_mysql.helper.Student')
    belongs_to('tests.integration.for_mysql.helper.Course')


class Student(AbsModel):
    has_many(Score)
    has_many('tests.integration.for_mysql.helper.Course', through=Score)


class Course(AbsModel):
    has_many(Score)
    has_many(Student, through=Score)


class StudentForHasOneThrough(AbsModel):
    __tablename__ = 'students'
    has_one('tests.integration.for_mysql.helper.ScoreForHasOneThrough', name='score', fk='student_id')
    has_one('tests.integration.for_mysql.helper.CourseForHasOneThrough', name="course", through="tests.integration.for_mysql.helper.ScoreForHasOneThrough", through_fk_on_owner='student_id', through_fk_on_target='course_id')


class CourseForHasOneThrough(AbsModel):
    __tablename__ = 'courses'
    has_one('tests.integration.for_mysql.helper.ScoreForHasOneThrough', name='score', fk='course_id')
    has_one(StudentForHasOneThrough, name="student", through="tests.integration.for_mysql.helper.ScoreForHasOneThrough", through_fk_on_owner='course_id', through_fk_on_target='student_id')


class ScoreForHasOneThrough(AbsModel):
    __tablename__ = 'scores'
    belongs_to(StudentForHasOneThrough, name='student', fk='student_id')
    belongs_to(CourseForHasOneThrough, name='course', fk='course_id')

