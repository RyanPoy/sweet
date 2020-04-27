# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
# from sweet.tests import helper
from sweet.tests.integration.test_recordset_crud_mysql import RecordsetCRUDMySQLTest
from sweet.tests.integration.test_model_struct_mysql import ModelStructMySQLTest
from sweet.tests.integration.test_model_crud_mysql import ModelCRUDMySQLTest

from sweet.tests.unit.test_filter import FilterTest
from sweet.tests.unit.test_clauses import ClausesTest

from sweet.tests.unit.test_recordset_query_for_mysql import MySQLRecordsetQueryTest
from sweet.tests.unit.test_recordset_insert_for_mysql import MySQLRecordsetInsertTest
from sweet.tests.unit.test_recordset_delete_for_mysql import MySQLRecordsetDeleteTest
from sweet.tests.unit.test_recordset_update_for_mysql import MySQLRecordsetUpdateTest

from sweet.tests.unit.test_model_define import ModelDefineTest

from sweet.tests.unit.test_validator_acceptance import AcceptanceValidatorTest
from sweet.tests.unit.test_validator_confirmation import ConfirmationValidatorTest
from sweet.tests.unit.test_validator_exclusion import ExclusionValidatorTest
from sweet.tests.unit.test_validator_format import FormatValidatorTest
from sweet.tests.unit.test_validator_inclusion import InclusionValidatorTest
from sweet.tests.unit.test_validator_length import LengthValidatorTest
from sweet.tests.unit.test_validator_numericality import NumericalityValidatorTest
from sweet.tests.unit.test_validator_presence import PresenceValidatorTest

from sweet.tests.unit.test_template_loader import LoaderTest
from sweet.tests.unit.test_template_basic import BasicTest
from sweet.tests.unit.test_template_if import IfTest
from sweet.tests.unit.test_template_for import ForTest
from sweet.tests.unit.test_template_include import IncludeTest
from sweet.tests.unit.test_template_extends import ExtendsTest
from sweet.tests.unit.test_template_complex import ComplexTest
from sweet.tests.unit.test_template_using import UsingTest

from sweet.tests.unit.test_form import TestForm
from sweet.tests.unit.test_form_button import TestFormButton
from sweet.tests.unit.test_form_checkbox import TestFormCheckbox
from sweet.tests.unit.test_form_color import TestFormColor
from sweet.tests.unit.test_form_date import TestFormDate
from sweet.tests.unit.test_form_datetime import TestFormDatetime
from sweet.tests.unit.test_form_email import TestFormEmail
from sweet.tests.unit.test_form_file import TestFormFile
from sweet.tests.unit.test_form_hidden import TestFormHidden
from sweet.tests.unit.test_form_label import TestFormLabel
from sweet.tests.unit.test_form_month import TestFormMonth
from sweet.tests.unit.test_form_number import TestFormNumber
from sweet.tests.unit.test_form_password import TestFormPassword
from sweet.tests.unit.test_form_radio import TestFormRadio
from sweet.tests.unit.test_form_range import TestFormRange
from sweet.tests.unit.test_form_search import TestFormSearch
from sweet.tests.unit.test_form_submit import TestFormSubmit
from sweet.tests.unit.test_form_tel import TestFormTel
from sweet.tests.unit.test_form_text import TestFormText
from sweet.tests.unit.test_form_textarea import TestFormTextarea
from sweet.tests.unit.test_form_time import TestFormTime
from sweet.tests.unit.test_form_url import TestFormUrl
from sweet.tests.unit.test_form_week import TestFormWeek


unit_tests = [
    FilterTest,
    ClausesTest,

    MySQLRecordsetQueryTest,
    MySQLRecordsetInsertTest,
    MySQLRecordsetDeleteTest,
    MySQLRecordsetUpdateTest,

    ModelDefineTest,
    
    AcceptanceValidatorTest,
    ConfirmationValidatorTest,
    ExclusionValidatorTest,
    FormatValidatorTest,
    InclusionValidatorTest,
    LengthValidatorTest,
    NumericalityValidatorTest,
    PresenceValidatorTest,
]

# template unit tests
unit_tests += [
    LoaderTest,
    ExtendsTest,
    BasicTest,
    IfTest,
    ForTest,
    IncludeTest,
    ComplexTest,
    UsingTest,
]

# form tests
unit_tests += [
    TestForm,
    TestFormButton,
    TestFormCheckbox,
    TestFormColor,
    TestFormDate,
    TestFormDatetime,
    TestFormEmail,
    TestFormFile,
    TestFormHidden,
    TestFormLabel,
    TestFormMonth,
    TestFormNumber,
    TestFormPassword,
    TestFormRadio,
    TestFormRange,
    TestFormSearch,
    TestFormSubmit,
    TestFormTel,
    TestFormText,
    TestFormTextarea,
    TestFormTime,
    TestFormUrl,
    TestFormWeek,
]

integration_tests = [
    RecordsetCRUDMySQLTest,
    ModelStructMySQLTest,
    ModelCRUDMySQLTest,
]


if __name__ == '__main__':
    cmd = sys.argv[1] if len(sys.argv) == 2 else 'all'
    if cmd not in ('all', 'unit', 'integration'):
        print ('python %s [all|unit|integration]' % sys.argv[0])
        sys.exit(-1)

    suite = unittest.TestSuite()
    if cmd == 'all' or cmd == 'unit':
        for t in unit_tests:
            suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    if cmd == 'all' or cmd == 'integration':
        for t in integration_tests:
            suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))

    unittest.TextTestRunner().run(suite)
