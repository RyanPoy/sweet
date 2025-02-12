import unittest
from datetime import datetime

from sweet.sequel.terms.name_fn import Sum, Name
from sweet.sequel.types import Array, Raw


class TestRaw(unittest.TestCase):

    def test_init_error(self):
        customerObj = object()
        with self.assertRaises(TypeError) as ctx:
            Raw(customerObj)
        self.assertEqual("Raw initialize accept typing.Union[NoneType, bool, str, bytes, int, float, decimal.Decimal, datetime.date, datetime.datetime], "
                         "but got 'object'", str(ctx.exception))


class TestArray(unittest.TestCase):

    def test_init(self):
        fn = Sum("age")
        name = Name("username")
        now = datetime.now()
        array = Array([1, "1", now, fn, name, [1, "1", now, fn, name, [1, "1", now, fn, name]]])
        self.assertEqual(array.data, [Raw(1), Raw("1"), Raw(now), fn, name, [Raw(1), Raw("1"), Raw(now), fn, name, [Raw(1), Raw("1"), Raw(now), fn, name]]])


if __name__ == '__main__':
    unittest.main()

