#coding: utf8
import unittest
from test_activerecord_group_and_having import ActiveRecordGroupAndHavingTest
from test_activerecord_join_and_include import ActiveRecordJoinAndIncludeTest
from test_activerecord_create_and_save import ActiveRecordCreateAndSaveTest
from test_activerecord_delete import ActiveRecordDeleteTest
from test_activerecord_update import ActiveRecordUpdateTest
from test_activerecord_where import ActiveRecordWhereTest
from test_activerecord_order import ActiveRecordOrderTest
from test_activerecord_limit import ActiveRecordLimitTest
from test_activerecord_init import ActiveRecordInitTest
from test_activerecord_find import ActiveRecordFindTest
from test_activerecord_func import ActiveRecordFuncTest
from test_association_belongs_to import BelongsToTest
from test_association_has_one import HasOneTest
from test_sql_builder import SQLBuilderTest


tests = [
    SQLBuilderTest,

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
    
    ActiveRecordJoinAndIncludeTest,

    BelongsToTest,
    HasOneTest,
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
