#coding: utf8
from sweet.tests import TestCase
from sweet.orm.model import Model
from sweet.orm.relations import *
import inspect


class TestRelationBasic(TestCase):

    def test_belongs_to_without_argument(self):

        class Member(Model):
            __tablename__ = 'users'

        class Phone(Model):
            __tablename__ = 'mobiles'
            belongs_to('member', Member)

        r = Phone.__relations__.get('member')
        self.assertEqual(BelongsTo, type(r))
        self.assertEqual(Phone, r.owner)
        self.assertEqual(Member, r.target)
        self.assertEqual('member_id', r.owner_fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('member', r.name)
        
    def test_belongs_to_with_argument(self):

        class Member(Model):
            __pk__ = 'member_id'
            __tablename__ = 'users'

        class Phone(Model):
            __tablename__ = 'mobiles'
            belongs_to('member', Member, fk='owner_id')

        r = Phone.__relations__.get('member')
        self.assertEqual(BelongsTo, type(r))
        self.assertEqual(Phone, r.owner)
        self.assertEqual(Member, r.target)
        self.assertEqual('owner_id', r.owner_fk)
        self.assertEqual('member_id', r.pk)    
        self.assertEqual('member', r.name)

    def test_has_many_without_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_many('phones', Phone)

        r = Member.__relations__.get('phones')
        self.assertEqual(HasMany, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('member_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('phones', r.name)

    def test_has_many_with_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_many('phones', Phone, fk='user_id')

        r = Member.__relations__.get('phones')
        self.assertEqual(HasMany, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('user_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('phones', r.name)

    def test_has_one_without_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_one('phone', Phone)

        r = Member.__relations__.get('phone')
        self.assertEqual(HasOne, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('member_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('phone', r.name)

    def test_has_one_with_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_one('phone', Phone, fk='user_id')

        r = Member.__relations__.get('phone')
        self.assertEqual(HasOne, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('user_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('phone', r.name)

    def test_has_and_belongs_to_many_without_argument(self):

        class Student(Model):
            pass

        class Teacher(Model):
            has_and_belongs_to_many('students', Student)

        HasAndBelongsToMany(name='teachers', target=Teacher).set_owner(Student)

        r = Teacher.__relations__.get('students')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Teacher, r.owner)
        self.assertEqual(Student, r.target)
        self.assertEqual('teacher_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('students', r.name)
        self.assertEqual('students_teachers', r.through_table)

        r = Student.__relations__.get('teachers')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Student, r.owner)
        self.assertEqual(Teacher, r.target)
        self.assertEqual('student_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('teachers', r.name)
        self.assertEqual('students_teachers', r.through_table)

    def test_has_and_belongs_to_many_with_argument(self):

        class StudentAndTeacher(Model):
            __tablename__ = 'student_teacher_relation'

        class Student(Model):
            pass

        class Teacher(Model):
            has_and_belongs_to_many('students', Student, through=StudentAndTeacher)

        HasAndBelongsToMany(name='teachers', target=Teacher, through=StudentAndTeacher).set_owner(Student)

        r = Teacher.__relations__.get('students')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Teacher, r.owner)
        self.assertEqual(Student, r.target)
        self.assertEqual('teacher_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('students', r.name)
        self.assertEqual('student_teacher_relation', r.through_table)

        r = Student.__relations__.get('teachers')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Student, r.owner)
        self.assertEqual(Teacher, r.target)
        self.assertEqual('student_id', r.fk)
        self.assertEqual('id', r.pk)
        self.assertEqual('teachers', r.name)
        self.assertEqual('student_teacher_relation', r.through_table)


if __name__ == '__main__':
    import unittest
    unittest.main()

