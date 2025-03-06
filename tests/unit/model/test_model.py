import unittest

from tests.helper import User


class TestModel(unittest.TestCase):

    def test_table_name(self):
        self.assertEqual("users", User.table.name)


if __name__ == '__main__':
    unittest.main()
