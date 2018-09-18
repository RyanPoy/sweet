# coding: utf8
import sys, unittest
from sweet.tests.unit.query.test_mysql_sqlbuilder import MysqlSQLBuilderTest
from sweet.tests.unit.orm.test_model import ModelTest
from sweet.tests.unit.validation.test_validator_acceptance import AcceptanceValidatorTest
from sweet.tests.unit.validation.test_validator_confirmation import ConfirmationValidatorTest
from sweet.tests.unit.validation.test_validator_exclusion import ExclusionValidatorTest
from sweet.tests.unit.validation.test_validator_format import FormatValidatorTest
from sweet.tests.unit.validation.test_validator_inclusion import InclusionValidatorTest
from sweet.tests.unit.validation.test_validator_length import LengthValidatorTest
from sweet.tests.unit.validation.test_validator_numericality import NumericalityValidatorTest
from sweet.tests.unit.validation.test_validator_presence import PresenceValidatorTest


unit_tests = [
    MysqlSQLBuilderTest,
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
