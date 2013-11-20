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
from pyrails.activerecord import ActiveRecord, has_and_belongs_to_many
from pyrails.tests import create_table, drop_table
import unittest


class Student(ActiveRecord): 
    has_and_belongs_to_many('pyrails.tests.test_association_has_and_belongs_to_many.Teacher')

class Teacher(ActiveRecord):
    has_and_belongs_to_many(Student)


class HasAndBelongsToManyTest(unittest.TestCase):
    
    def setUp(self):
        drop_table('students_teachers')
        drop_table('students')
        drop_table('teachers')

        create_table("""
create table if not exists students (
    id int auto_increment,
    name varchar(32) not null,
    PRIMARY KEY (id)
);
""")
        create_table("""
create table if not exists teachers (
    id int auto_increment,
    created_at datetime,
    user_id int,
    PRIMARY KEY (id)
);
""")
        create_table("""
create table if not exists students_teachers (
    id int auto_increment,
    student_id int,
    teacher_id int,
    created_at datetime,
    user_id int,
    PRIMARY KEY (id)
);
""")

    def tearDown(self):
        drop_table('students')
        drop_table('teachers')
        
    def test_has_and_belongs_to_many(self):
        self.assertEqual(1, len(Student.association_dict))
        association = Student.association_of('teachers')
        self.assertTrue(association.is_has_and_belongs_to_many())
        self.assertEqual('teachers', association.attr_name)
        self.assertEqual('teacher_id', association.foreign_key)
        self.assertEqual('students_teachers', association.join_table)
        self.assertFalse(association.dependent)

        self.assertEqual(1, len(Teacher.association_dict))
        association = Teacher.association_of('students')
        self.assertTrue(association.is_has_and_belongs_to_many())
        self.assertEqual('students', association.attr_name)
        self.assertEqual('student_id', association.foreign_key)
        self.assertEqual('students_teachers', association.join_table)
        self.assertFalse(association.dependent)
        
    # def test_has_and_belongs_to_many_create(self):
    #     fid = Father.create(name='Bob').id
    #     cid = Child.create(created_at='2011-10-10 12:12:12', father_id=fid).id
    #     c = Child.find(cid)
    #     self.assertEqual(cid, c.id)
    #     self.assertEqual(fid, c.father_id)
    #     self.assertEqual('2011-10-10 12:12:12', c.created_at.strftime('%Y-%m-%d %H:%M:%S'))


if __name__ == '__main__':
    unittest.main()
