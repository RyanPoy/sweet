import unittest
from unittest import TestLoader, TestSuite

from tests.unit.sequel.statements.test_delete_statement import TestDeleteStatement
from tests.unit.sequel.statements.test_insert_statement import TestInsertStatement
from tests.unit.sequel.statements.test_select_statement import TestSelectStatement
from tests.unit.sequel.statements.test_update_statement import TestUpdateStatement
from tests.unit.sequel.terms.test_fn import TestFn
from tests.unit.sequel.terms.test_lock import TestLock
from tests.unit.sequel.terms.test_name import TestName
from tests.unit.sequel.terms.test_q import TestQ
from tests.unit.sequel.terms.test_values import TestValues
from tests.unit.sequel.test_raw import TestArray, TestRaw


if __name__ == '__main__':
    loader = TestLoader()
    suites = TestSuite()
    for case in [
        TestDeleteStatement,
        TestInsertStatement,
        TestUpdateStatement,
        TestSelectStatement,
        TestQ,
        TestLock,
        TestValues,
        TestName,
        TestFn,
        TestRaw,
        TestArray,
    ]:
        suites.addTests(loader.loadTestsFromTestCase(case))
    unittest.TextTestRunner().run(suites)
