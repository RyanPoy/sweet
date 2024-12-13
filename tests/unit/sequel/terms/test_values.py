import unittest
from datetime import datetime

from sweet.sequel.terms.values import Values


class TestValues(unittest.TestCase):

    def test_values(self):
        d1 = datetime(2024, 12, 13, 11, 9, 28, 547992)
        d2 = datetime(2024, 12, 13, 11, 9, 28, 547992)
        d3 = datetime(2024, 12, 13, 11, 9, 28, 547992)

        vs = Values(1, "lucy", 30, d1)
        self.assertEqual([(1, "lucy", 30, d1)], vs.data)

        vs.append(2, "lily", 20, d2)
        self.assertEqual([(1, "lucy", 30, d1), (2, "lily", 20, d2)], vs.data)

        vs.append_list((3, "jim", 12, d3), (4, "noname", 9, d1))
        self.assertEqual([(1, "lucy", 30, d1), (2, "lily", 20, d2), (3, "jim", 12, d3), (4, "noname", 9, d1)], vs.data)


if __name__ == '__main__':
    unittest.main()
