#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from tests.integration.for_sqlite.helper import Student, Course, Score


class TestRelationHasManyThroughSQLite(unittest.TestCase):

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
        Score.create(student=s1, course=c2, value=90)
        Score.create(student=s2, course=c1, value=95)

        self.assertEqual(3, len(Score.all()))

        cs = s1.courses.all()
        self.assertEqual(2, len(cs))
        self.assertEqual('math', cs[0].name)
        self.assertEqual('sport', cs[1].name)

        cs = s2.courses.all()
        self.assertEqual(1, len(cs))
        self.assertEqual('math', cs[0].name)

        ss = c1.students.all()
        self.assertEqual(2, len(ss))
        self.assertEqual('lily', ss[0].name)
        self.assertEqual('jon', ss[1].name)

        ss = c2.students.all()
        self.assertEqual(1, len(ss))
        self.assertEqual('lily', ss[0].name)

    def test_query_with_include(self):
        s1 = Student.create(name='lily')
        s2 = Student.create(name='jon')

        c1 = Course.create(name='math')
        c2 = Course.create(name='sport')

        Score.create(student=s1, course=c1, value=100)
        Score.create(student=s1, course=c2, value=90)
        Score.create(student=s2, course=c1, value=95)

        self.assertEqual(3, len(Score.include('student', 'course').all()))

        cs = Student.include('courses').where(name='lily').first().courses.all()
        self.assertEqual(2, len(cs))
        self.assertEqual('math', cs[0].name)
        self.assertEqual('sport', cs[1].name)

        cs = Student.include('courses').where(name='jon').first().courses.all()
        self.assertEqual(1, len(cs))
        self.assertEqual('math', cs[0].name)

        ss = Course.include('students').where(name='math').first().students.all()
        self.assertEqual(2, len(ss))
        self.assertEqual('lily', ss[0].name)
        self.assertEqual('jon', ss[1].name)

        ss = Course.include('students').where(name='sport').first().students.all()
        self.assertEqual(1, len(ss))
        self.assertEqual('lily', ss[0].name)

    def test_dissociate(self):
        s1 = Student.create(name='lily')

        c1 = Course.create(name='math')
        c2 = Course.create(name='swim')
        c3 = Course.create(name='boxing')

        Score.create(student=s1, course=c1, value=100)
        Score.create(student=s1, course=c2, value=90)
        Score.create(student=s1, course=c3, value=95)

        self.assertEqual(3, len(Score.all()))
        self.assertEqual(3, len(s1.courses.all()))

        s1.dissociate_with_courses(c1, c2)
        self.assertEqual(1, len(Score.all()))
        self.assertEqual(1, len(s1.courses.all()))
        self.assertEqual('boxing', s1.courses.first().name)


if __name__ == '__main__':
    unittest.main()
