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

from tests.forms.test_form import FormTest
from tests.forms.test_button import ButtonTest
from tests.forms.test_checkbox import CheckboxTest
from tests.forms.test_color import ColorTest
from tests.forms.test_date import DateTest
from tests.forms.test_datetime import DatetimeTest
from tests.forms.test_email import EmailTest
from tests.forms.test_file import FileTest
from tests.forms.test_hidden import HiddenTest
from tests.forms.test_label import LabelTest
from tests.forms.test_month import MonthTest
from tests.forms.test_number import NumberTest
from tests.forms.test_password import PasswordTest
from tests.forms.test_text import TextTest
from tests.forms.test_tel import TelTest
from tests.forms.test_radio import RadioTest
from tests.forms.test_range import RangeTest
from tests.forms.test_search import SearchTest
from tests.forms.test_submit import SubmitTest
from tests.forms.test_textarea import TextareaTest
from tests.forms.test_time import TimeTest
from tests.forms.test_url import UrlTest
from tests.forms.test_week import WeekTest


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
    ColorTest,
    DateTest,
    DatetimeTest,
    EmailTest,
    FileTest,
    HiddenTest,
    LabelTest,
    MonthTest,
    NumberTest,
    PasswordTest,
    TelTest,
    RadioTest,
    RangeTest,
    SearchTest,
    SubmitTest,
    TextareaTest,
    TextTest,
    TimeTest,
    UrlTest,
    WeekTest,
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
