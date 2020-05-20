#coding: utf8
from sweet._tests import TestCase, Student, Course, Score
from sweet.orm import atomic


class TestTransactionMysql(TestCase):

    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Score.delete_all()
        Student.delete_all()
        Course.delete_all()

    def test_atomic_transaction_successful(self):

        @atomic
        def insert():
            s1 = Student.create(name='lily')
            s2 = Student.create(name='jon')

            c1 = Course.create(name='math')
            c2 = Course.create(name='sport')

            Score.create(student=s1, course=c1, value=100)
            Score.create(student=s1, course=c2, value=90)
            Score.create(student=s2, course=c1, value=95)

        insert()

        self.assertEqual(2, Student.count())
        self.assertEqual(2, Course.count())
        self.assertEqual(3, Score.count())


    def test_atomic_without_db_transaction_successful(self):

        @atomic()
        def insert():
            s1 = Student.create(name='lily')
            s2 = Student.create(name='jon')

            c1 = Course.create(name='math')
            c2 = Course.create(name='sport')

            Score.create(student=s1, course=c1, value=100)
            Score.create(student=s1, course=c2, value=90)
            Score.create(student=s2, course=c1, value=95)

        insert()

        self.assertEqual(2, Student.count())
        self.assertEqual(2, Course.count())
        self.assertEqual(3, Score.count())

    def test_atomic_with_db_transaction_successful(self):

        @atomic(Student.db)
        def insert():
            s1 = Student.create(name='lily')
            s2 = Student.create(name='jon')

            c1 = Course.create(name='math')
            c2 = Course.create(name='sport')

            Score.create(student=s1, course=c1, value=100)
            Score.create(student=s1, course=c2, value=90)
            Score.create(student=s2, course=c1, value=95)

        insert()

        self.assertEqual(2, Student.count())
        self.assertEqual(2, Course.count())
        self.assertEqual(3, Score.count())

    def test_atomic_transaction_failed(self):

        @atomic
        def insert():
            s1 = Student.create(name='lily')
            s2 = Student.create(name='jon')

            c1 = Course.create(name='math')
            c2 = Course.create(name='sport')

            Score.create(student=s1, course=c1, value=100)
            Score.create(student=s1, course=c2, value=90)
            Score.create(student=s2, course=c1, value=95)

            raise Exeption("Fake Exception")

        with self.assertRaises(Exception) as err:
            insert()
            self.assertEqual("Fake Exception", str(err.exception))

        self.assertEqual(0, Student.count())
        self.assertEqual(0, Course.count())
        self.assertEqual(0, Score.count())

    def test_manual_transaction_with(self):
        with Student.transaction() as t:
            s1 = Student.create(name='lily')
            s2 = Student.create(name='jon')
            t.commit()

            c1 = Course.create(name='math')
            c2 = Course.create(name='sport')

            t.commit()

            Score.create(student=s1, course=c1, value=100)
            Score.create(student=s1, course=c2, value=90)
            Score.create(student=s2, course=c1, value=95)
            t.rollback()

        self.assertEqual(2, Student.count())
        self.assertEqual(2, Course.count())
        self.assertEqual(0, Score.count())
        

if __name__ == '__main__':
    import unittest
    unittest.main()
