import unittest
from unittest import TestLoader, TestSuite

from tests.unit.sequel.schema.test_table import TestTable
from tests.unit.sequel.statements.test_delete_statement import TestDeleteStatement
from tests.unit.sequel.statements.test_insert_statement import TestInsertStatement
from tests.unit.sequel.statements.test_select_statement import TestSelectStatement
from tests.unit.sequel.statements.test_update_statement import TestUpdateStatement
from tests.unit.sequel.terms.test_condition import TestCondition
from tests.unit.sequel.terms.test_q import TestQ
from tests.unit.sequel.terms.test_values import TestValues


if __name__ == '__main__':
    loader = TestLoader()
    suites = TestSuite()
    for case in [
        TestTable,
        TestDeleteStatement,
        TestInsertStatement,
        TestUpdateStatement,
        TestSelectStatement,
        TestCondition,
        TestQ,
        TestValues,
    ]:
        suites.addTests(loader.loadTestsFromTestCase(case))
    unittest.TextTestRunner().run(suites)
