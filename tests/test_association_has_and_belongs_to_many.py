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
        drop_developers_projects()
        drop_projects()
        drop_developers()
        
    def test_has_and_belongs_to_many(self):
        self.assertEqual(1, len(Developer.association_dict))
        association = Developer.association_of('projects')
        self.assertTrue(association.is_has_and_belongs_to_many())
        self.assertEqual('projects', association.attr_name)
        self.assertEqual('project_id', association.foreign_key)
        self.assertEqual('developers_projects', association.join_table)
        self.assertFalse(association.dependent)

        association = Project.association_of('developers')
        self.assertTrue(association.is_has_and_belongs_to_many())
        self.assertEqual('developers', association.attr_name)
        self.assertEqual('developer_id', association.foreign_key)
        self.assertEqual('developers_projects', association.join_table)
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
