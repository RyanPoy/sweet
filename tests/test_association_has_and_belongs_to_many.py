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


class HasAndBelongsToManyTest(unittest.TestCase):
    
    def setUp(self):
        drop_developers_projects()
        drop_projects()
        drop_developers()
        
        create_projects()
        create_developers()
        create_developers_projects()

    def tearDown(self):
        # drop_developers_projects()
        # drop_projects()
        # drop_developers()
        pass
        
    # def test_has_and_belongs_to_many(self):
    #     self.assertEqual(1, len(Developer.association_dict))
    #     association = Developer.association_of('projects')
    #     self.assertTrue(association.is_has_and_belongs_to_many())
    #     self.assertEqual('projects', association.attr_name)
    #     self.assertEqual('developer_id', association.foreign_key)
    #     self.assertEqual('project_id', association.association_foreign_key)
    #     self.assertEqual('developers_projects', association.join_table)
    #     self.assertFalse(association.dependent)

    #     association = Project.association_of('developers')
    #     self.assertTrue(association.is_has_and_belongs_to_many())
    #     self.assertEqual('developers', association.attr_name)
    #     self.assertEqual('project_id', association.foreign_key)
    #     self.assertEqual('developer_id', association.association_foreign_key)
    #     self.assertEqual('developers_projects', association.join_table)
    #     self.assertFalse(association.dependent)

    # def test_has_and_belongs_to_many_with_some_customer_init_args(self):
    #     class Developer(ActiveRecord): has_and_belongs_to_many(Project, foreign_key='user_id')
    #     self.assertEqual(1, len(Developer.association_dict))
    #     association = Developer.association_of('projects')
    #     self.assertTrue(association.is_has_and_belongs_to_many())
    #     self.assertEqual('projects', association.attr_name)
    #     self.assertEqual('user_id', association.foreign_key)
    #     self.assertEqual('project_id', association.association_foreign_key)
    #     self.assertEqual('developers_projects', association.join_table)
    #     self.assertFalse(association.dependent)

    # def test_has_and_belongs_to_many_with_more_customer_init_args(self):
    #     class Developer(ActiveRecord): has_and_belongs_to_many(Project, attr_name='users', foreign_key='user_id', join_table='pro_dev')
    #     self.assertEqual(1, len(Developer.association_dict))
    #     association = Developer.association_of('users')
    #     self.assertTrue(association.is_has_and_belongs_to_many())
    #     self.assertEqual('users', association.attr_name)
    #     self.assertEqual('user_id', association.foreign_key)
    #     self.assertEqual('project_id', association.association_foreign_key)
    #     self.assertEqual('pro_dev', association.join_table)
    #     self.assertFalse(association.dependent)

    def test_has_and_belongs_to_many_find(self):
        """ Developer#projects (similar to Project.joins('developers_projects').where(developer_id=id))
        """
        class DeveloperProject(ActiveRecord): __table_name__ = 'developers_projects'

        dev1 = Developer(name="dev1").save()
        dev2 = Developer(name="dev2").save()
        dev3 = Developer(name="dev3").save()

        pro1 = Project(name="pro1").save()
        pro2 = Project(name="pro2").save()
        pro3 = Project(name="pro3").save()

        dev_pro1 = DeveloperProject(developer_id=dev1.id, project_id=pro1.id).save()
        dev_pro2 = DeveloperProject(developer_id=dev1.id, project_id=pro2.id).save()
        dev_pro3 = DeveloperProject(developer_id=dev1.id, project_id=pro3.id).save()
        dev_pro10 = DeveloperProject(developer_id=5, project_id=pro1.id).save()

        dev_pro4 = DeveloperProject(developer_id=dev2.id, project_id=pro2.id).save()
        dev_pro5 = DeveloperProject(developer_id=dev2.id, project_id=pro3.id).save()
        
        dev_pro6 = DeveloperProject(developer_id=dev3.id, project_id=pro3.id).save()

        projects = dev1.projects
        self.assertEqual(3, len(projects))
        self.assertEqual('pro1', projects[0].name)
        self.assertEqual('pro2', projects[1].name)
        self.assertEqual('pro3', projects[2].name)

        developers = pro2.developers
        self.assertEqual(2, len(developers))
        self.assertEqual('dev1', developers[0].name)
        self.assertEqual('dev2', developers[1].name)

    # def test_has_and_belongs_to_many_build(self):
    #     """ Firm#clients.build (similar to Client(firm_id=id))
    #     """
    #     firm = Firm(name="firm1").save()
    #     client = firm.clients.build(name="client1")
    #     self.assertEqual('client1', client.name)
    #     self.assertEqual(1, client.firm_id)
    #     self.assertIsNone(client.id)
        
    # def test_has_and_belongs_to_many_create(self):
    #     """ Firm#clients.create (similar to c = Client(firm_id=id); c.save(); return c)
    #     """
    #     firm = Firm(name="firm1").save()
    #     client = firm.clients.create(name="client1")
    #     self.assertEqual('client1', client.name)
    #     self.assertEqual(1, client.firm_id)
    #     self.assertEqual(1, client.id)

    # def test_has_and_belongs_to_many_create(self):
    #     fid = Father.create(name='Bob').id
    #     cid = Child.create(created_at='2011-10-10 12:12:12', father_id=fid).id
    #     c = Child.find(cid)
    #     self.assertEqual(cid, c.id)
    #     self.assertEqual(fid, c.father_id)
    #     self.assertEqual('2011-10-10 12:12:12', c.created_at.strftime('%Y-%m-%d %H:%M:%S'))


if __name__ == '__main__':
    unittest.main()
