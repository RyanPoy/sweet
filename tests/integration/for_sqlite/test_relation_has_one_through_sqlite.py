#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from tests.integration.for_sqlite.helper import StudentForHasOneThrough as Student
from tests.integration.for_sqlite.helper import CourseForHasOneThrough as Course
from tests.integration.for_sqlite.helper import ScoreForHasOneThrough as Score


class TestRelationHasOneThroughSQLite(unittest.TestCase):

    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Score.delete_all()
        Student.delete_all()
        Course.delete_all()

    def test_query(self):
        s1 = Student.create(name='lily')
        s2 = Student.create(name='jon')

        c1 = Course.create(name='math')
        c2 = Course.create(name='sport')

        Score.create(student=s1, course=c1, value=100)
        Score.create(student=s2, course=c2, value=90)

        self.assertEqual(2, len(Score.all()))

        self.assertEqual('math', s1.course.name)
        self.assertEqual('sport', s2.course.name)

        self.assertEqual('lily', c1.student.name)
        self.assertEqual('jon', c2.student.name)

    def test_query_with_include(self):
        s1 = Student.create(name='lily')
        s2 = Student.create(name='jon')

        c1 = Course.create(name='math')
        c2 = Course.create(name='sport')

        Score.create(student=s1, course=c1, value=100)
        Score.create(student=s2, course=c2, value=90)

        self.assertEqual(2, len(Score.include('student', 'course').all()))

        ss = Student.include('score', 'course').all()
        s1, s2 = ss[0], ss[1]
        self.assertEqual('math', s1.course.name)
        self.assertEqual('sport', s2.course.name)

        cs = Course.include('score', 'student').all()
        c1, c2 = cs[0], cs[1]
        self.assertEqual('lily', c1.student.name)
        self.assertEqual('jon', c2.student.name)

    def test_dissociate(self):
        s1 = Student.create(name='lily')
        c1 = Course.create(name='math')
        Score.create(student=s1, course=c1, value=100)

        self.assertEqual(1, len(Score.all()))

        s1.dissociate_with_course(c1)
        self.assertEqual(0, len(Score.all()))
        self.assertEqual(None, s1.course)
        self.assertEqual(None, c1.student)


if __name__ == '__main__':
    unittest.main()
