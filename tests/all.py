#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
from tests.test_template import TemplateTest
from tests.test_loader import LoaderTest
from tests.test_parse import ParseTest
from tests.test_basic_template import BasicTemplateTest
from tests.test_if_template import IfTemplateTest
from tests.test_for_template import ForTemplateTest


tests = [
    TemplateTest,
    LoaderTest,
    ParseTest,
    BasicTemplateTest,
    IfTemplateTest,
    ForTemplateTest
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
