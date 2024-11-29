#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from tests.integration.for_sqlite.helper import Foo
from sweet.record.fields import *


class TestModelStructSQLite(unittest.TestCase):
    
    def test_table_columns(self):
        field_dict = Foo.__field_define_dict__

        f = field_dict.get('c_bigint')
        self.assertEqual(IntField, type(f))
        self.assertEqual('c_bigint', f.name)
        self.assertEqual(32, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(10, f.default)

        f = field_dict.get('c_blob')
        self.assertEqual(BlobField, type(f))
        self.assertEqual('c_blob', f.name)
        self.assertEqual(1024, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_bool')
        self.assertEqual(BoolField, type(f))
        self.assertEqual('c_bool', f.name)
        self.assertEqual(1, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(False, f.default)

        f = field_dict.get('c_char')
        self.assertEqual(CharField, type(f))
        self.assertEqual('c_char', f.name)
        self.assertEqual(32, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual("'c'", f.default)

        f = field_dict.get('c_date')
        self.assertEqual(DateField, type(f))
        self.assertEqual('c_date', f.name)
        self.assertEqual(False, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_datetime')
        self.assertEqual(DatetimeField, type(f))
        self.assertEqual('c_datetime', f.name)
        self.assertEqual(False, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_double')
        self.assertEqual(FloatField, type(f))
        self.assertEqual('c_double', f.name)
        self.assertEqual(False, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_integer')
        self.assertEqual(IntField, type(f))
        self.assertEqual('c_integer', f.name)
        self.assertEqual(32, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(100, f.default)

        f = field_dict.get('c_numeric')
        self.assertEqual(DecimalField, type(f))
        self.assertEqual('c_numeric', f.name)
        self.assertEqual(10, f.precision)
        self.assertEqual(0, f.scale)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_real')
        self.assertEqual(FloatField, type(f))
        self.assertEqual('c_real', f.name)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_smallint')
        self.assertEqual(IntField, type(f))
        self.assertEqual('c_smallint', f.name)
        self.assertEqual(32, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_text')
        self.assertEqual(TextField, type(f))
        self.assertEqual('c_text', f.name)
        self.assertEqual(1024, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_time')
        self.assertEqual(DatetimeField, type(f))
        self.assertEqual('c_time', f.name)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_varbinary')
        self.assertEqual(BlobField, type(f))
        self.assertEqual('c_varbinary', f.name)
        self.assertEqual(255, f.length)
        self.assertEqual(True, f.null)
        self.assertEqual(None, f.default)

        f = field_dict.get('c_varchar')
        self.assertEqual(CharField, type(f))
        self.assertEqual('c_varchar', f.name)
        self.assertEqual(64, f.length)
        self.assertEqual(False, f.null)
        self.assertEqual("''", f.default)


if __name__ == '__main__':
    unittest.main()
