import unittest
from datetime import date, datetime, time
from decimal import Decimal

from sweet.model.columns import BinaryColumn, BooleanColumn, CharColumn, DateColumn, DatetimeColumn, DecimalColumn, FloatColumn, IntColumn, TextColumn, \
    TimeColumn
from sweet.model import Model


class TestColumn(unittest.TestCase):

    def test_unavailable_length(self):
        with self.assertRaises(ValueError) as ex:
            CharColumn(length=-10)
        self.assertEqual("The length must be greater than zero, but got -10", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            TextColumn(length=0)
        self.assertEqual('The length must be greater than zero, but got 0', str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            BinaryColumn(length=-10)

    def test_name_is_not_none_when_defined_in_model(self):
        class Demo(Model):
            id = IntColumn()
            name = CharColumn("demo_name")

        self.assertEqual('demo_name', Demo.table.columns.name.name)
        self.assertEqual('id', Demo.table.columns.id.name)

        self.assertEqual('demo_name', Demo.columns.name.name)
        self.assertEqual('id', Demo.columns.id.name)

    def test_value(self):
        col = CharColumn()
        col.value = "abc"
        self.assertEqual("abc", col.value)

        col = TextColumn()
        col.value = "abc"
        self.assertEqual("abc", col.value)

        col = BinaryColumn()
        col.value = "abc"
        self.assertEqual(b"abc", col.value)

        col = IntColumn()
        col.value = 10
        self.assertEqual(10, col.value)

        col = BooleanColumn()
        col.value = "y"
        self.assertTrue(col.value)

        col.value = "n"
        self.assertFalse(col.value)

        col = FloatColumn()
        col.value = " 12.33"
        self.assertEqual(12.33, col.value)

        col = DecimalColumn()
        col.value = "12.45"
        self.assertEqual(Decimal("12.45"), col.value)

        col = DateColumn()
        col.value = "2016-10-9"
        self.assertEqual(date(2016, 10, 9), col.value)

        col = DatetimeColumn()
        col.value = "2016-10-9 11:12:3"
        self.assertEqual(datetime(2016, 10, 9, 11, 12, 3), col.value)

        col = TimeColumn()
        col.value = "11:12:3"
        self.assertEqual(time(11, 12, 3), col.value)

    def test_value_error(self):
        with self.assertRaises(ValueError) as ex:
            BinaryColumn().value = 19
        self.assertEqual("Can't purify 19, it's a int type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            IntColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            FloatColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            DecimalColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            DateColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            DatetimeColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))

        with self.assertRaises(ValueError) as ex:
            TimeColumn().value = "abc"
        self.assertEqual("Can't purify abc, it's a str type.", str(ex.exception))


if __name__ == '__main__':
    unittest.main()
