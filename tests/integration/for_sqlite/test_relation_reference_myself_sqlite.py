#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from tests.integration.for_sqlite.helper import Category


class TestRelationReferenceMyselfSQLite(unittest.TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Category.delete_all()

    def test_create(self):
        c_root = Category.create(name="根目录")
        c_1 = Category.create(parent=c_root, name='目录-1')
        c_1_1 = Category.create(parent=c_1, name='目录-1-1')
        c_1_2 = Category.create(parent=c_1, name='目录-1-2')

        c_2 = Category.create(parent=c_root, name='目录-1')
        c_2_1 = Category.create(parent=c_2, name='目录-2-1')
        c_2_2 = Category.create(parent=c_2, name='目录-2-2')

        children = c_root.children.all()
        self.assertEqual(2, len(children))

        child_1, child_2 = children[0], children[1]
        self.assertEqual(c_1.id, child_1.id)
        self.assertEqual(c_2.id, child_2.id)

        children = child_1.children.all()
        self.assertEqual(2, len(children))
        self.assertEqual(c_1_1.id, children[0].id)
        self.assertEqual(c_1_2.id, children[1].id)

        children = child_2.children.all()
        self.assertEqual(2, len(children))
        self.assertEqual(c_2_1.id, children[0].id)
        self.assertEqual(c_2_2.id, children[1].id)

    def test_save(self):
        c_root = Category.create(name="根目录")
        c_1 = Category.create(name='目录-1')
        c_1.parent = c_root
        c_1.save()

        c_1_1 = Category.create(name='目录-1-1')
        c_1_1.parent = c_1
        c_1_1.save()

        c_1_2 = Category.create(name='目录-1-2')
        c_1_2.parent = c_1
        c_1_2.save()

        c_2 = Category(parent=c_root, name='目录-1').save()
        c_2_1 = Category.create(parent=c_2, name='目录-2-1').save()
        c_2_2 = Category.create(parent=c_2, name='目录-2-2').save()

        children = c_root.children.all()
        self.assertEqual(2, len(children))

        child_1, child_2 = children[0], children[1]
        self.assertEqual(c_1.id, child_1.id)
        self.assertEqual(c_2.id, child_2.id)

        children = child_1.children.all()
        self.assertEqual(2, len(children))
        self.assertEqual(c_1_1.id, children[0].id)
        self.assertEqual(c_1_2.id, children[1].id)

        children = child_2.children.all()
        self.assertEqual(2, len(children))
        self.assertEqual(c_2_1.id, children[0].id)
        self.assertEqual(c_2_2.id, children[1].id)

    def test_update(self):
        c_root = Category.create(name="根目录")
        c_1 = Category.create(parent=c_root, name='目录-1')
        c_1_1 = Category.create(parent=c_1, name='目录-1-1')
        c_1_2 = Category.create(parent=c_1, name='目录-1-2')

        c_2 = Category.create(parent=c_root, name='目录-1')
        c_2_1 = Category.create(parent=c_2, name='目录-2-1')
        c_2_2 = Category.create(parent=c_2, name='目录-2-2')

        
        c_2_1.parent = c_1
        c_2_1.save()

        c_2_2.update(parent=c_1)

        children = c_1.children.all()
        self.assertEqual(4, len(children))
        self.assertEqual(c_1_1.id, children[0].id)
        self.assertEqual(c_1_2.id, children[1].id)
        self.assertEqual(c_2_1.id, children[2].id)
        self.assertEqual(c_2_2.id, children[3].id)


if __name__ == '__main__':
    unittest.main()
