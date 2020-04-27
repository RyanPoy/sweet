# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import unittest
# from sweet.tests import helper
from sweet.tests.integration.test_recordset_crud_mysql import TestRecordsetCRUDMySQL
from sweet.tests.integration.test_model_struct_mysql import TestModelStructMySQL
from sweet.tests.integration.test_model_crud_mysql import TestModelCRUDMySQL

from sweet.tests.unit.test_filter import TestFilter
from sweet.tests.unit.test_clauses import TestClauses

from sweet.tests.unit.test_recordset_query_for_mysql import TestRecordsetQueryForMySQL
from sweet.tests.unit.test_recordset_insert_for_mysql import TestRecordsetInsertForMySQL
from sweet.tests.unit.test_recordset_delete_for_mysql import TestRecordsetDeleteForMysql
from sweet.tests.unit.test_recordset_update_for_mysql import TestRecordsetUpdateMySQL

from sweet.tests.unit.test_model_define import TestModelDefine

from sweet.tests.unit.test_validator_acceptance import TestValidatorAcceptance
from sweet.tests.unit.test_validator_confirmation import TestValidatorConfirmation
from sweet.tests.unit.test_validator_exclusion import TestValidatorExclusion
from sweet.tests.unit.test_validator_format import TestValidatorFormat
from sweet.tests.unit.test_validator_inclusion import TestValidatorInclusion
from sweet.tests.unit.test_validator_length import TestValidatorLength
from sweet.tests.unit.test_validator_numericality import TestValidatorNumericality
from sweet.tests.unit.test_validator_presence import TestValidatorPresence

from sweet.tests.unit.test_template_basic import TestTemplateBasic
from sweet.tests.unit.test_template_complex import TestTemplateComplex
from sweet.tests.unit.test_template_extends import TestTemplateExtends
from sweet.tests.unit.test_template_for import TestTemplateFor
from sweet.tests.unit.test_template_if import TestTemplateIf
from sweet.tests.unit.test_template_include import TestTemplateInclude
from sweet.tests.unit.test_template_loader import TestTemplateLoader
from sweet.tests.unit.test_template_using import TestTemplateUsing

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
    TestFilter,
    TestClauses,

    TestRecordsetQueryForMySQL,
    TestRecordsetInsertForMySQL,
    TestRecordsetDeleteForMysql,
    TestRecordsetUpdateMySQL,

    TestModelDefine,
    
    TestValidatorAcceptance,
    TestValidatorConfirmation,
    TestValidatorExclusion,
    TestValidatorFormat,
    TestValidatorInclusion,
    TestValidatorLength,
    TestValidatorNumericality,
    TestValidatorPresence,
]

# template unit tests
unit_tests += [
    TestTemplateExtends,
    TestTemplateBasic,
    TestTemplateComplex,
    TestTemplateFor,
    TestTemplateIf,
    TestTemplateInclude,
    TestTemplateLoader,
    TestTemplateUsing,
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
    TestRecordsetCRUDMySQL,
    TestModelStructMySQL,
    TestModelCRUDMySQL,
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
