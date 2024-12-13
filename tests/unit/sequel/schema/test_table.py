import unittest

from sweet.sequel.schema.table import Table


class TestTable(unittest.TestCase):

    def test_init(self):
        t = Table("users")
        self.assertEqual("users", t.name)
        self.assertEqual('"users"', t.name_quoted)


if __name__ == '__main__':
    unittest.main()

