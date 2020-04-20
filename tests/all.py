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

from tests.form.test_form import FormTest
from tests.form.test_button import ButtonTest
from tests.form.test_checkbox import CheckboxTest
from tests.form.test_color_field import ColorFieldTest
from tests.form.test_date_field import DateFieldTest
from tests.form.test_datetime_field import DatetimeFieldTest
from tests.form.test_email_field import EmailFieldTest
from tests.form.test_file_field import FileFieldTest
from tests.form.test_hidden_field import HiddenFieldTest
from tests.form.test_label import LabelTest
from tests.form.test_month_field import MonthTest


tests = [
    LoaderTest,
    ExtendsTest,
    BasicTest,
    IfTest,
    ForTest,
    IncludeTest,
    ComplexTest,
    UsingTest,

    FormTest,
    ButtonTest,
    CheckboxTest,
    ColorFieldTest,
    DateFieldTest,
    DatetimeFieldTest,
    EmailFieldTest,
    FileFieldTest,
    HiddenFieldTest,
    LabelTest,
    MonthTest    
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
