import unittest

from sweet.sequel.nodes.insert_statement import InsertStatement


class TestInsertStatement(unittest.TestCase):

    def test_clone(self):
        statement = InsertStatement()
        statement.columns = ["a", "b", "c"]
        statement.values = ["x", "y", "z"]

        dolly = statement.clone()

        self.assertEqual(dolly.columns, statement.columns)
        self.assertEqual(dolly.values, statement.values)

        self.assertIsNot(dolly.columns, statement.columns)
        self.assertIsNot(dolly.values, statement.values)

    def test_is_equal_with_equal_ivars(self):
        statement1 = InsertStatement()
        statement1.columns = ["a", "b", "c"]
        statement1.values = ["x", "y", "z"]

        statement2 = InsertStatement()
        statement2.columns = ["a", "b", "c"]
        statement2.values = ["x", "y", "z"]

        array = { statement1, statement2 }
        self.assertEqual(1, len(array))


if __name__ == '__main__':
    print("*"*10)
    unittest.main()
    print("*"*10)
