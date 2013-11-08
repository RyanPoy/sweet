# -*- coding:utf-8 -*-
import unittest
from test_activerecord_init import ActiveRecordInitTest
from test_activerecord_create_and_save import ActiveRecordCreateAndSaveTest
from test_activerecord_delete import ActiveRecordDeleteTest
from test_activerecord_update import ActiveRecordUpdateTest
from test_activerecord_find import ActiveRecordFindTest
from test_activerecord_where import ActiveRecordWhereTest
from test_activerecord_group_and_having import ActiveRecordGroupAndHavingTest


tests = [
    ActiveRecordInitTest,
    ActiveRecordCreateAndSaveTest,
    ActiveRecordDeleteTest,
    ActiveRecordUpdateTest,
    ActiveRecordFindTest,
    ActiveRecordWhereTest,
    ActiveRecordGroupAndHavingTest,
]


if __name__ == '__main__':
    suite = unittest.TestSuite()
    for t in tests:
        suite.addTest(unittest.defaultTestLoader.loadTestsFromTestCase(t))
    unittest.TextTestRunner().run(suite)
