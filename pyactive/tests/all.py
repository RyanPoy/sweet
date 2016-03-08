#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
import unittest
from pyactive.tests.test_criteria_insert import CriteriaInsertTestCase
from pyactive.tests.test_criteria_delete import CriteriaDeleteTestCase
from pyactive.tests.test_criteria_update import CriteriaUpdateTestCase
from pyactive.tests.test_criteria_query import CriteriaQueryTestCase
from pyactive.tests.test_criteria_aggregate_functions import CriteriaAggregateFunctionTestCase 

from pyactive.tests.test_record import RecordTestCase
from pyactive.tests.test_record_base import RecordBaseTestCase
from pyactive.tests.test_record_insert import RecordInsertTestCase
from pyactive.tests.test_record_update import RecordUpdateTestCase
from pyactive.tests.test_record_delete import RecordDeleteTestCase
from pyactive.tests.test_record_query import RecordQueryTestCase
from pyactive.tests.test_record_method_missing import RecordMethodMissingTestCase

from pyactive.tests.test_relation_belongs_to import RelationBelongsToTestCase
from pyactive.tests.test_relation_has_one import RelationHasOneTestCase
from pyactive.tests.test_relation_has_many import RelationHasManyTestCase
from pyactive.tests.test_relation_has_and_belongs_to_many import RelationHasAndBelongsToManyTestCase


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
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
