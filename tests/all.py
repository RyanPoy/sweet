#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
from tests.test_loader import LoaderTest

from tests.test_basic import BasicTest
from tests.test_if import IfTest
from tests.test_for import ForTest
from tests.test_include import IncludeTest
from tests.test_extends import ExtendsTest
from tests.test_complex import ComplexTest
from tests.test_using import UsingTest

from tests.test_tag_form import TagFormTest
from tests.test_tag_form_button import TagFormButtonTest
from tests.test_tag_form_checkbox import TagFormCheckboxTest
from tests.test_tag_form_color_field import TagFormColorFieldTest
from tests.test_tag_form_date_field import TagFormDateFieldTest
from tests.test_tag_form_datetime_field import TagFormDatetimeFieldTest
from tests.test_tag_form_email_field import TagFormEmailFieldTest


tests = [
    LoaderTest,
    ExtendsTest,
    BasicTest,
    IfTest,
    ForTest,
    IncludeTest,
    ComplexTest,
    UsingTest,

    TagFormTest,
    TagFormButtonTest,
    TagFormCheckboxTest,
    TagFormColorFieldTest,
    TagFormDateFieldTest,
    TagFormDatetimeFieldTest,
    TagFormEmailFieldTest
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
