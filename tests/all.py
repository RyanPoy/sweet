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
from tests.form.test_form_button import FormButtonTest
from tests.form.test_form_checkbox import FormCheckboxTest
from tests.form.test_form_color_field import FormColorFieldTest
from tests.form.test_form_date_field import FormDateFieldTest
from tests.form.test_form_datetime_field import FormDatetimeFieldTest
from tests.form.test_form_email_field import FormEmailFieldTest
from tests.form.test_form_file_field import FormFileFieldTest
from tests.form.test_form_hidden_field import FormHiddenFieldTest
from tests.form.test_form_label import FormLabelTest
from tests.form.test_form_month_field import FormMonthTest


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
    FormButtonTest,
    FormCheckboxTest,
    FormColorFieldTest,
    FormDateFieldTest,
    FormDatetimeFieldTest,
    FormEmailFieldTest,
    FormFileFieldTest,
    FormHiddenFieldTest,
    FormLabelTest,
    FormMonthTest    
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
