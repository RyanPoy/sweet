import unittest

from sweet.sequel.terms.name_fn import Name
from tests.helper import User


class TestModel(unittest.TestCase):

    def test_table_name(self):
        self.assertEqual("users", User.table.name)
        self.assertEqual(Name("users"), User.table.name_named)


if __name__ == '__main__':
    unittest.main()
