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
from test_activerecord_group_and_having import ActiveRecordGroupAndHavingTest
from test_activerecord_create_and_save import ActiveRecordCreateAndSaveTest
from test_activerecord_delete import ActiveRecordDeleteTest
from test_activerecord_update import ActiveRecordUpdateTest
from test_activerecord_where import ActiveRecordWhereTest
from test_activerecord_order import ActiveRecordOrderTest
from test_activerecord_limit import ActiveRecordLimitTest
from test_activerecord_joins import ActiveRecordJoinsTest
from test_activerecord_init import ActiveRecordInitTest
from test_activerecord_find import ActiveRecordFindTest
from test_activerecord_func import ActiveRecordFuncTest
from test_association_has_and_belongs_to_many import HasAndBelongsToManyTest
from test_association_belongs_to import BelongsToTest
from test_association_has_many import HasManyTest
from test_association_has_one import HasOneTest
from test_collection import CollectionTest


tests = [
    CollectionTest,

    ActiveRecordInitTest,
    ActiveRecordCreateAndSaveTest,
    ActiveRecordDeleteTest,
    ActiveRecordUpdateTest,
    ActiveRecordFindTest,
    ActiveRecordWhereTest,
    ActiveRecordGroupAndHavingTest,
    ActiveRecordOrderTest,
    ActiveRecordLimitTest,
    ActiveRecordFuncTest,
    ActiveRecordJoinsTest,

    HasAndBelongsToManyTest,
    BelongsToTest,
    HasOneTest,
    HasManyTest,
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
