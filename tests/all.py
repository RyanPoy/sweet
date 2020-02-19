#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
from tests.test_loader import LoaderTest
from tests.test_parse import ParseTest

from tests.test_basic import BasicTest
from tests.test_if import IfTest
from tests.test_for import ForTest
from tests.test_include import IncludeTest

tests = [
    LoaderTest,
    ParseTest,
    BasicTest,
    IfTest,
    ForTest,
    IncludeTest
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
