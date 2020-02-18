#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
from tests.test_template import TemplateTest
from tests.test_loader import FileLoaderTest, MemLoaderTest
from tests.test_parse import ParseTest


tests = [
    TemplateTest,
    FileLoaderTest,
    MemLoaderTest,
    ParseTest
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
