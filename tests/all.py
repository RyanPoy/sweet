#coding: utf8
import unittest
from sweet.tests.units.test_criteria_insert import CriteriaInsertTestCase
from sweet.tests.units.test_criteria_delete import CriteriaDeleteTestCase
from sweet.tests.units.test_criteria_update import CriteriaUpdateTestCase
from sweet.tests.units.test_criteria_query import CriteriaQueryTestCase
from sweet.tests.units.test_criteria_aggregate_functions import CriteriaAggregateFunctionTestCase 

# from sweet.tests.units.test_record import RecordTestCase
from sweet.tests.units.test_record_base import RecordBaseTestCase
from sweet.tests.units.test_record_insert import RecordInsertTestCase
from sweet.tests.units.test_record_update import RecordUpdateTestCase
from sweet.tests.units.test_record_delete import RecordDeleteTestCase
from sweet.tests.units.test_record_query import RecordQueryTestCase
from sweet.tests.units.test_record_method_missing import RecordMethodMissingTestCase

from sweet.tests.units.test_relation_belongs_to import RelationBelongsToTestCase
from sweet.tests.units.test_relation_has_one import RelationHasOneTestCase
from sweet.tests.units.test_relation_has_many import RelationHasManyTestCase
from sweet.tests.units.test_relation_has_and_belongs_to_many import RelationHasAndBelongsToManyTestCase

from sweet.tests.units.test_validator_acceptance import AcceptanceValidatorTest
from sweet.tests.units.test_validator_confirmation import ConfirmationValidatorTest
from sweet.tests.units.test_validator_format import FormatValidatorTest
from sweet.tests.units.test_validator_inclusion import InclusionValidatorTest
from sweet.tests.units.test_validator_length import LengthValidatorTest
from sweet.tests.units.test_validator_exclusion import ExclusionValidatorTest
from sweet.tests.units.test_validator_numericality import NumericalityValidatorTest
from sweet.tests.units.test_validator_presence import PresenceValidatorTest


tests = [
    CriteriaAggregateFunctionTestCase,
    CriteriaInsertTestCase,
    CriteriaDeleteTestCase,
    CriteriaUpdateTestCase,
    CriteriaQueryTestCase,

    RecordBaseTestCase,
    RecordUpdateTestCase,
    RecordInsertTestCase,
    RecordDeleteTestCase,
    RecordQueryTestCase,
    RecordMethodMissingTestCase,

    RelationBelongsToTestCase,
    RelationHasOneTestCase,
    RelationHasManyTestCase,
    RelationHasAndBelongsToManyTestCase,
    
    AcceptanceValidatorTest,
    ConfirmationValidatorTest,
    FormatValidatorTest,
    InclusionValidatorTest,
    LengthValidatorTest,
    ExclusionValidatorTest,
    NumericalityValidatorTest,
    PresenceValidatorTest,
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
