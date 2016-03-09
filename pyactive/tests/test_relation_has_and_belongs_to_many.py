# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from ..relation import has_and_belongs_to_many, HasAndBelongsToMany
from datetime import datetime
import unittest
import fudge

 
class Category(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at', 'name']
 
 
class RelationHasAndBelongsToManyTestCase(unittest.TestCase):
 
    def test_has_and_belongs_to_many_relation_init(self):
        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
     
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']
  
        r = HasAndBelongsToMany(target_class=Student, owner_class=Teacher, foreign_key='teacher_id', 
                                owner_attr='students', target_foreign_key='student_id', association_table='teachers_students')
        self.assertTrue(r.owner is Teacher)
        self.assertEqual('teacher_id', r.foreign_key)
        self.assertTrue(r.target is Student)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('students', r.owner_attr)
        self.assertEqual('teachers_students', r.association_table)
        self.assertEqual('student_id', r.target_foreign_key)
     
    def test_has_and_belongs_to_many_relation_init_without_target_foreign_key(self):
        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
     
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']
  
        r = HasAndBelongsToMany(target_class=Student, owner_class=Teacher, foreign_key='teacher_id', 
                                owner_attr='students', association_table='teachers_students')
        self.assertTrue(r.owner is Teacher)
        self.assertEqual('teacher_id', r.foreign_key)
        self.assertTrue(r.target is Student)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('students', r.owner_attr)
        self.assertEqual('teachers_students', r.association_table)
        self.assertEqual('student_id', r.target_foreign_key)
         
    def test_has_and_belongs_to_many_relation_init_without_association_table(self):
        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
     
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']
  
        r = HasAndBelongsToMany(target_class=Student, owner_class=Teacher, foreign_key='teacher_id', owner_attr='students')
        self.assertTrue(r.owner is Teacher)
        self.assertEqual('teacher_id', r.foreign_key)
        self.assertTrue(r.target is Student)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('students', r.owner_attr)
        self.assertEqual('students_teachers', r.association_table)
        self.assertEqual('student_id', r.target_foreign_key)
  
    def test_has_and_belongs_to_many_relation_init_without_owner_attr(self):
        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
     
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']
  
        r = HasAndBelongsToMany(target_class=Student, owner_class=Teacher, foreign_key='teacher_id')
        self.assertTrue(r.owner is Teacher)
        self.assertEqual('teacher_id', r.foreign_key)
        self.assertTrue(r.target is Student)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('students', r.owner_attr)
        self.assertEqual('students_teachers', r.association_table)
        self.assertEqual('student_id', r.target_foreign_key)
   
    def test_has_and_belongs_to_many_relation_init_without_foreign_key(self):
        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
     
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']
  
        r = HasAndBelongsToMany(target_class=Student, owner_class=Teacher, owner_attr='students')
        self.assertTrue(r.owner is Teacher)
        self.assertEqual('teacher_id', r.foreign_key)
        self.assertTrue(r.target is Student)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('students', r.owner_attr)
        self.assertEqual('students_teachers', r.association_table)
        self.assertEqual('student_id', r.target_foreign_key)
#           
#     def test_has_and_belongs_to_many_relation_init_should_raise_exception_if_relation_foreign_key_not_in_columns(self):
#         class User(ActiveRecord):
#             __columns__ = ['id', 'created_at', 'updated_at']
#   
#         class Phone(ActiveRecord):
#             __columns__ = ['id', 'created_at', 'updated_at']
#   
#         r = HasAndBelongsToMany(target_class=User, owner_class=Phone)
#         self.assertRaises(ColumnNotInColumns, lambda: r.foreign_key)
   
    def test_has_and_belongs_to_many_relation_init_with_target_classpath(self):
        class Video(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'name']
 
        r = HasAndBelongsToMany(target_class="pyactive.tests.test_relation_has_and_belongs_to_many.Category", owner_class=Video)
        self.assertTrue(r.owner is Video)
        self.assertEqual('video_id', r.foreign_key)
        self.assertTrue(r.target is Category)
        self.assertEqual('id', r.target_pk_column)
        self.assertEqual('categories', r.owner_attr)
        self.assertEqual('categories_videos', r.association_table)
        self.assertEqual('category_id', r.target_foreign_key)

    @fudge.patch('pyactive.record.ar.Criteria',
                 'pyactive.relation.r_has_and_belongs_to_many.JoinClause')
    def test_teacher_has_and_belongs_to_many_student_relation(self, Criteria, JoinClause):
        class Student(ActiveRecord):
            __columns__ = ['id', 'age', 'created_at', 'updated_at']

        class Teacher(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_and_belongs_to_many(Student)
    
        Criteria.is_callable().returns_fake()\
                .expects('from_').with_args('teachers').returns_fake()\
                .expects('where').with_args(id=(1, )).returns_fake()\
                .expects('first').returns(Teacher(id=1, name="poy", created_at=datetime.now(), updated_at=datetime.now()))
        t = Teacher.find(1)
         
        JoinClause.is_callable().with_args('students_teachers').returns_fake()\
                  .expects('on').with_args('`students_teachers`.`teacher_id` = ?', 1).returns_fake()\
                  .expects('on').with_args('`students_teachers`.`student_id` = `students`.`id`').returns_fake()
 
        Criteria.is_callable().returns_fake()\
                .expects('from_').with_args('students').returns_fake()\
                .expects('join').returns_fake()\
                .expects('all').returns([
                    Student(id=11, age=10, created_at=datetime.now(), updated_at=datetime.now()),
                    Student(id=12, age=20, created_at=datetime.now(), updated_at=datetime.now()),
                    Student(id=13, age=30, created_at=datetime.now(), updated_at=datetime.now()),
                ])
        students = t.students
        self.assertEqual(3, len(students))
        s = students[1]
        self.assertEqual(20, s.age)
        self.assertEqual(12, s.id)
        self.assertTrue(isinstance(s.created_at, datetime))
        self.assertTrue(isinstance(s.updated_at, datetime))


if __name__ == '__main__':
    unittest.main()
