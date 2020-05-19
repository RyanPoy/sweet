#coding: utf8
from sweet._tests import TestCase
from template import FileLoader, MemLoader
import os


class TestTemplateLoader(TestCase):
    
    def setUp(self):
        self.abs_dir = os.path.dirname(os.path.abspath(__file__))
        self.loader = FileLoader(root_abs_dir=os.path.join(self.abs_dir, 'root'))
    
    def test_abs_root_path(self):
        expected = os.path.join(self.abs_dir, 'root')
        self.assertEqual(expected, self.loader.root_dir)

    def test_build_base_path(self):
        expected = os.path.join(self.abs_dir, 'root/level-1/index.html')
        self.assertEqual(expected, self.loader.build_path('level-1/index.html'))
    
    def test_build_path_with_parent_path(self):
        expected = os.path.join(self.abs_dir, 'root/level-1/_partial.html')
        parent_path = self.loader.build_path('level-1/index.html')
        self.assertEqual(expected,
                         self.loader.build_path('_partial.html', parent_path=parent_path)
        )
         
    def test_build_path_with_parent_path2(self):
        expected = os.path.join(self.abs_dir, 'root/_partial.html')
        parent_path = self.loader.build_path('level-1/index.html')
        self.assertEqual(expected,
                         self.loader.build_path('../_partial.html', parent_path=parent_path)
        )
         
    def test_build_path_with_parent_path3(self):
        expected = os.path.join(self.abs_dir, 'root/level-1/level-2/_partial.html')
        parent_path = self.loader.build_path('level-1/index.html')
        self.assertEqual(expected,
                         self.loader.build_path('level-2/_partial.html', parent_path=parent_path)
        )
    
    def test_mem_load_build_path(self):
        self.assertEqual("parent.html", MemLoader({"parent.html": "aaaaaa"}).build_path("parent.html"))


if __name__ == '__main__':
    import unittest
    unittest.main()

