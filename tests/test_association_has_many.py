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
from pyrails.activerecord import ActiveRecord
from pyrails.tests.test_association_helper import *
import unittest


class HasManyTest(unittest.TestCase):

    def setUp(self):
        drop_clients()
        drop_firms()

        create_firms()
        create_clients()

    def tearDown(self):
        drop_clients()
        drop_firms()

    def test_has_many(self):
        self.assertEqual(1, len(Firm.association_dict))
        association = Firm.association_of('clients')
        self.assertTrue(association.is_has_many())
        self.assertEqual(Client, association.target)
        self.assertEqual('clients', association.attr_name)
        self.assertEqual('firm_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_many_with_some_customer_init_args(self):
        class Firm(ActiveRecord): has_many(Client, attr_name="customers")
        self.assertEqual(1, len(Firm.association_dict))
        association = Firm.association_of('customers')
        self.assertTrue(association.is_has_many())
        self.assertEqual(Client, association.target)
        self.assertEqual('customers', association.attr_name)
        self.assertEqual('firm_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_many_with_more_customer_init_args(self):
        class Firm(ActiveRecord): has_many(Client, attr_name="customers", foreign_key="customer_id")
        self.assertEqual(1, len(Firm.association_dict))
        association = Firm.association_of('customers')
        self.assertTrue(association.is_has_many())
        self.assertEqual(Client, association.target)
        self.assertEqual('customers', association.attr_name)
        self.assertEqual('customer_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_many_find(self):
        """ Firm#clients (similar to Client.where(firm_id: id))
        """
        firm = Firm(name="firm1").save()
        Client(name="client1", firm_id=firm.id).save()
        Client(name="client2", firm_id=firm.id).save()
        Client(name="client3", firm_id=firm.id).save()
        Client(name="client4", firm_id=firm.id).save()
        clients = Firm.find(1).clients
        self.assertEqual(4, len(clients))
        self.assertEqual(1, clients[0].id)
        self.assertEqual(2, clients[1].id)
        self.assertEqual(3, clients[2].id)
        self.assertEqual(4, clients[3].id)

        client = Firm.find(1).clients.find(2)
        self.assertEqual(2, client.id)
        self.assertEqual('client2', client.name)
    
    def test_has_many_build(self):
        """ Firm#clients.build (similar to Client(firm_id=id))
        """
        firm = Firm(name="firm1").save()
        client = firm.clients.build(name="client1")
        self.assertEqual('client1', client.name)
        self.assertEqual(1, client.firm_id)
        self.assertIsNone(client.id)
        
    def test_has_many_create(self):
        """ Firm#clients.create (similar to c = Client(firm_id=id); c.save(); return c)
        """
        firm = Firm(name="firm1").save()
        client = firm.clients.create(name="client1")
        self.assertEqual('client1', client.name)
        self.assertEqual(1, client.firm_id)
        self.assertEqual(1, client.id)

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
    #             name varchar(32) not null,
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
