# coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import logging
logging.basicConfig(
    level=logging.DEBUG, 
    format="%(asctime)s - %(name)s [%(levelname)s]: %(message)s",
)

import unittest
from sweet._tests.integration.test_recordset_crud_mysql import TestRecordsetCRUDMySQL
from sweet._tests.integration.test_model_struct_mysql import TestModelStructMySQL
from sweet._tests.integration.test_model_crud_mysql import TestModelCRUDMySQL
from sweet._tests.integration.test_relation_belongs_to_mysql import TestRelationBelongsToMysql
from sweet._tests.integration.test_relation_reference_myself_mysql import TestRelationReferenceMysqlMysql
from sweet._tests.integration.test_relation_has_many_mysql import TestRelationHasManyToMysql
from sweet._tests.integration.test_relation_has_many_through_mysql import TestRelationHasManyThroughMysql
from sweet._tests.integration.test_relation_has_one_mysql import TestHasOneToMysql
from sweet._tests.integration.test_relation_has_one_through_mysql import TestRelationHasOneThroughMysql
from sweet._tests.integration.test_relation_has_and_belongs_to_many_mysql import TestRelationHasAndBelongsToManyMysql
from sweet._tests.integration.test_relation_include_mysql import TestRelationIncludeMysql
from sweet._tests.integration.test_transaction_mysql import TestTransactionMysql

from sweet._tests.unit.test_utils import TestUtils
from sweet._tests.unit.test_inflection import TestInflection
from sweet._tests.unit.test_collection import TestCollection
from sweet._tests.unit.test_filter import TestFilter
from sweet._tests.unit.test_clauses import TestClauses

from sweet._tests.unit.test_recordset_query_for_mysql import TestRecordsetQueryForMySQL
from sweet._tests.unit.test_recordset_insert_for_mysql import TestRecordsetInsertForMySQL
from sweet._tests.unit.test_recordset_delete_for_mysql import TestRecordsetDeleteForMysql
from sweet._tests.unit.test_recordset_update_for_mysql import TestRecordsetUpdateMySQL

from sweet._tests.unit.test_model_define import TestModelDefine

from sweet._tests.unit.test_validator_acceptance import TestValidatorAcceptance
from sweet._tests.unit.test_validator_confirmation import TestValidatorConfirmation
from sweet._tests.unit.test_validator_exclusion import TestValidatorExclusion
from sweet._tests.unit.test_validator_format import TestValidatorFormat
from sweet._tests.unit.test_validator_inclusion import TestValidatorInclusion
from sweet._tests.unit.test_validator_length import TestValidatorLength
from sweet._tests.unit.test_validator_numericality import TestValidatorNumericality
from sweet._tests.unit.test_validator_presence import TestValidatorPresence

from sweet._tests.unit.test_template_basic import TestTemplateBasic
from sweet._tests.unit.test_template_complex import TestTemplateComplex
from sweet._tests.unit.test_template_extends import TestTemplateExtends
from sweet._tests.unit.test_template_for import TestTemplateFor
from sweet._tests.unit.test_template_if import TestTemplateIf
from sweet._tests.unit.test_template_include import TestTemplateInclude
from sweet._tests.unit.test_template_loader import TestTemplateLoader
from sweet._tests.unit.test_template_using import TestTemplateUsing

from sweet._tests.unit.test_form import TestForm
from sweet._tests.unit.test_form_button import TestFormButton
from sweet._tests.unit.test_form_checkbox import TestFormCheckbox
from sweet._tests.unit.test_form_color import TestFormColor
from sweet._tests.unit.test_form_date import TestFormDate
from sweet._tests.unit.test_form_datetime import TestFormDatetime
from sweet._tests.unit.test_form_email import TestFormEmail
from sweet._tests.unit.test_form_file import TestFormFile
from sweet._tests.unit.test_form_hidden import TestFormHidden
from sweet._tests.unit.test_form_label import TestFormLabel
from sweet._tests.unit.test_form_month import TestFormMonth
from sweet._tests.unit.test_form_number import TestFormNumber
from sweet._tests.unit.test_form_password import TestFormPassword
from sweet._tests.unit.test_form_radio import TestFormRadio
from sweet._tests.unit.test_form_range import TestFormRange
from sweet._tests.unit.test_form_search import TestFormSearch
from sweet._tests.unit.test_form_submit import TestFormSubmit
from sweet._tests.unit.test_form_tel import TestFormTel
from sweet._tests.unit.test_form_text import TestFormText
from sweet._tests.unit.test_form_textarea import TestFormTextarea
from sweet._tests.unit.test_form_time import TestFormTime
from sweet._tests.unit.test_form_url import TestFormUrl
from sweet._tests.unit.test_form_week import TestFormWeek

from sweet._tests.unit.test_relation_basic import TestRelationBasic



unit_tests = [
    TestUtils,
    TestInflection,
    TestCollection,
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

# relation tests
unit_tests += [
    TestRelationBasic
]

integration_tests = [
    TestTransactionMysql,

    TestRecordsetCRUDMySQL,
    TestModelStructMySQL,
    TestModelCRUDMySQL,

    TestRelationBelongsToMysql,
    TestRelationReferenceMysqlMysql,

    TestRelationHasManyToMysql,
    TestRelationHasManyThroughMysql,

    TestHasOneToMysql,
    TestRelationHasOneThroughMysql,

    TestRelationHasAndBelongsToManyMysql,

    TestRelationIncludeMysql
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
