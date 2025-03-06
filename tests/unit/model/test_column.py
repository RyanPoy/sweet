import unittest

from sweet.model.columns import BinaryColumn, CharColumn, IntColumn, TextColumn
from sweet.model.model import Model


class TestColumn(unittest.TestCase):

    def test_unavailable_length(self):
        with self.assertRaises(ValueError) as ex:
            CharColumn(length=-10)
        self.assertEqual('The length must be greater than zero, but got -10', str(ex.exception))

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


if __name__ == '__main__':
    unittest.main()
