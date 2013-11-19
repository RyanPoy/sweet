#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from pyrails.active_record import ActiveRecord, has_many, belongs_to
from pyrails.tests import drop_table, create_table
import unittest


class StudentTeacher(ActiveRecord):
    belongs_to('pyrails.tests.test_association_has_many.Teacher')
    belongs_to('pyrails.tests.test_association_has_many.Student')

class Student(ActiveRecord):
    has_many(StudentTeacher)
    has_many('pyrails.tests.test_association_has_many.Teacher', through='student_teachers')

class Teacher(ActiveRecord):
    has_many(StudentTeacher)
    has_many(Student, through='students')


class HasManyTest(unittest.TestCase):

    def setUp(self):
        drop_table('users')
        drop_table('cards')

        create_table("""
create table if not exists users (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")
        create_table("""
create table if not exists cards (
    id int auto_increment,
    created_at datetime,
    user_id int,
    PRIMARY KEY (id)
);
""")

    def tearDown(self):
        drop_table('users')
        drop_table('cards')

    def test_has_many(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_many(Card)

        self.assertEqual(1, len(User.association_dict))
        association = User.association_of('cards')
        self.assertTrue(association.is_has_many())
        self.assertEqual(Card, association.target)
        self.assertEqual('cards', association.attr_name)
        self.assertEqual('user_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_many_with_customer_init_args(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_many(Card, attr_name="mycards", foreign_key='uid', dependent=True)

        self.assertEqual(1, len(User.association_dict))
        association = User.association_of('mycards')
        self.assertTrue(association.is_has_many())
        self.assertEqual(Card, association.target)
        self.assertEqual('mycards', association.attr_name)
        self.assertEqual('uid', association.foreign_key)
        self.assertTrue(association.dependent)
    
    def test_find_a_record_after_has_many_create(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_many(Card)

        user = User.create(name='pengyi')
        card = user.create_card(created_at='2012-10-10 12:12:12')
        self.assertEqual(user.id, card.user_id)
        self.assertEqual(1, card.id)
        card = Card.find(1)
        self.assertEqual(1, card.id)
        self.assertEqual('2012-10-10 12:12:12', card.created_at.strftime('%Y-%m-%d %H:%M:%S'))

    def test_find_in_user_a_record_after_has_many_create(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_many(Card)
        
        user = User.create(name='pengyi')
        card = user.create_card(created_at='2012-10-10 12:12:12')
        self.assertEquals(card, user.card)

    def test_has_many_create_with_customer_attr_name(self):
        class Card(ActiveRecord): pass
        class User(ActiveRecord): has_many(Card)
        
        user = User.create(name='pengyi')
        card1 = Card.create(user_id=user.id, created_at='2012-10-10 12:12:12')
        card2 = user.cards[0]
        self.assertEqual(card1.id, card2.id)

    # def test_has_many_through(self):
    #     try:
    #         drop_table('student_teachers')
    #         drop_table('students')
    #         drop_table('teachers')

    #         create_table("""
    #         create table if not exists students (
    #             id int auto_increment,
    #             name varchar(32) not null,
    #             PRIMARY KEY (id)
    #         );
    #         """)

    #         create_table("""
    #         create table if not exists teachers (
    #             id int auto_increment,
    #             created_at datetime,
    #             age int,
    #             PRIMARY KEY (id)
    #         );
    #         """)

    #         create_table("""
    #         create table if not exists student_teachers (
    #             id int auto_increment,
    #             student_id int,
    #             teacher_id int,
    #             PRIMARY KEY (id)
    #         );
    #         """)
    #         s1 = Student.create(name="stu1")
    #         s2 = Student.create(name="stu2")
    #         s3 = Student.create(name="stu3")

    #         t1 = Teacher.create(name="tea1")
    #         t2 = Teacher.create(name="tea2")
    #         t3 = Teacher.create(name="tea3")

    #         StudentTeacher.create(student=s1, teacher=t1)
    #         StudentTeacher.create(student=s1, teacher=t2)
    #         StudentTeacher.create(student=s1, teacher=t3)

    #         StudentTeacher.create(student=s2, teacher=t2)
    #         StudentTeacher.create(student=s2, teacher=t3)

    #         ts = s1.teachers
    #         self.assertEqual(3, len(ts))
    #         self.assertEqual('tea1', ts[0].name)
    #         self.assertEqual('tea2', ts[1].name)
    #         self.assertEqual('tea3', ts[2].name)

    #         ts = s2.teachers
    #         self.assertEqual(2, len(ts))
    #         self.assertEqual('tea2', ts[0].name)
    #         self.assertEqual('tea3', ts[1].name)

    #         self.assertEqual(0, len(s3.teachers))

    #     finally:
    #         drop_table('student_teachers')
    #         drop_table('students')
    #         drop_table('teachers')


if __name__ == '__main__':
    unittest.main()
