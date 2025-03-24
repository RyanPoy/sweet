import unittest

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

    # def test_value(self):
    #     CharColumn().value = "abc"
    #     TextColumn().value = "abc"
    #     BinaryColumn().value = "abc"
    #     IntColumn().value = "abc"
    #     BooleanColumn().value = "abc"
    #     FloatColumn().value = "abc"
    #     DecimalColumn().value = "abc"
    #     DateColumn().value = "abc"
    #     DatetimeColumn().value = "abc"
    #     TimeColumn().value = "abc"

    def test_value_error(self):
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
