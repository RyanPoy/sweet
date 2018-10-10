# coding: utf8
import sys, unittest
from sweet.tests.integration.test_mysql import MySQLTest

from sweet.tests.unit.test_filter import FilterTest
from sweet.tests.unit.test_clauses import ClausesTest

from sweet.tests.unit.test_mysql_table_query import MySQLRecordSetQueryTest
from sweet.tests.unit.test_mysql_table_insert import MySQLRecordSetInsertTest
from sweet.tests.unit.test_mysql_table_delete import MySQLRecordSetDeleteTest
from sweet.tests.unit.test_mysql_table_update import MySQLRecordSetUpdateTest

from sweet.tests.unit.test_model import ModelTest

from sweet.tests.unit.test_validator_acceptance import AcceptanceValidatorTest
from sweet.tests.unit.test_validator_confirmation import ConfirmationValidatorTest
from sweet.tests.unit.test_validator_exclusion import ExclusionValidatorTest
from sweet.tests.unit.test_validator_format import FormatValidatorTest
from sweet.tests.unit.test_validator_inclusion import InclusionValidatorTest
from sweet.tests.unit.test_validator_length import LengthValidatorTest
from sweet.tests.unit.test_validator_numericality import NumericalityValidatorTest
from sweet.tests.unit.test_validator_presence import PresenceValidatorTest


unit_tests = [
    FilterTest,
    ClausesTest,

    MySQLRecordSetQueryTest,
    MySQLRecordSetInsertTest,
    MySQLRecordSetDeleteTest,
    MySQLRecordSetUpdateTest,

    ModelTest,
    
    AcceptanceValidatorTest,
    ConfirmationValidatorTest,
    ExclusionValidatorTest,
    FormatValidatorTest,
    InclusionValidatorTest,
    LengthValidatorTest,
    NumericalityValidatorTest,
    PresenceValidatorTest,
]


integration_tests = [
    MySQLTest    
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
