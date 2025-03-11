import unittest
from unittest import TestLoader, TestSuite

from tests.integration.sweet.database.test_mysql_driver import TestMySQLDriver
from tests.integration.sweet.database.test_postgres_driver import TestPostgreSQLDriver
from tests.integration.sweet.database.test_sqlite_driver import TestSQLiteDriver
from tests.integration.sweet.model.schema.test_columns import TestColumns
from tests.unit.model.test_column import TestColumn
from tests.unit.model.test_model import TestModel
from tests.unit.model.test_objects import TestObjects
from tests.unit.sequel.statements.test_delete_statement import TestDeleteStatement
from tests.unit.sequel.statements.test_insert_statement import TestInsertStatement
from tests.unit.sequel.statements.test_select_statement import TestSelectStatement
from tests.unit.sequel.statements.test_update_statement import TestUpdateStatement
from tests.unit.sequel.terms.test_binary import TestBinary
from tests.unit.sequel.terms.test_fn import TestFn
from tests.unit.sequel.terms.test_lock import TestLock
from tests.unit.sequel.terms.test_name import TestName
from tests.unit.sequel.terms.test_q import TestQ
from tests.unit.sequel.terms.test_values import TestValues
from tests.unit.sequel.test_raw import TestArray, TestRaw
from tests.unit.utils.test_inflection import TestInflection
from tests.unit.utils.test_utils import TestUtils


def main():
    loader = TestLoader()
    suites = TestSuite()
    for case in [
        # unit tests
        TestUtils,
        TestInflection,
        TestDeleteStatement,
        TestInsertStatement,
        TestUpdateStatement,
        TestSelectStatement,
        TestBinary,
        TestQ,
        TestLock,
        TestValues,
        TestName,
        TestFn,
        TestRaw,
        TestArray,
        TestColumn,

        # integration tests
        TestMySQLDriver,
        TestSQLiteDriver,
        TestPostgreSQLDriver,
        TestColumns,
        TestModel,
        TestObjects,
    ]:
        suites.addTests(loader.loadTestsFromTestCase(case))
    unittest.TextTestRunner().run(suites)


if __name__ == '__main__':
    main()
