import unittest

from sweet.sequel.types import Raw


class TestRaw(unittest.TestCase):

    def test_init_error(self):
        customerObj = object()
        with self.assertRaises(TypeError) as ctx:
            Raw(customerObj)
        self.assertEqual("Raw initialize accept typing.Union[NoneType, bool, str, bytes, int, float, decimal.Decimal, datetime.date, datetime.datetime], "
                         "but got 'object'", str(ctx.exception))


if __name__ == '__main__':
    unittest.main()

