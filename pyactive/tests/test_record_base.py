# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from ..utils import PKColumnNotInColumns
import unittest


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']


class RecordBaseTestCase(unittest.TestCase):
    
    def setUp(self):
        self.tbname = OrmRecord.__table_name__
    
    def tearDown(self):
        OrmRecord.__table_name__ = self.tbname 
        
    def test_table_name(self):
        self.assertEqual('orm_records', OrmRecord.table_name) 
        self.assertEqual('orm_records', OrmRecord.__table_name__)
        OrmRecord.__table_name__ = 'records'
        self.assertEqual('records', OrmRecord.table_name) 
        
    def test_init(self):
        r = OrmRecord({'name':'py', 'age':2}, age=3, d=4, b=5)
        self.assertEquals('py', r.name)
        self.assertEquals(3, r.age)
        self.assertEquals(4, r.d)
        self.assertEquals(5, r.b)
        self.assertFalse(r.is_dirty())

    def test_persisit_attrs(self):
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5, name="poy")
        self.assertEqual({'name': "poy", 'age': None, 'id': None}, r.persist_attrs(True))
        self.assertEqual({'name': "poy", 'age': None}, r.persist_attrs())

    def test_dirty_attrs(self):
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5, name="poy")
        r._ActiveRecord__origin_attrs = dict(name='ryan', age=10)
        self.assertTrue(r.is_dirty())
        self.assertFalse(r.is_dirty('b'))
        self.assertTrue(r.is_dirty('name'))
        self.assertTrue(r.is_dirty('age'))

    def test_sync_attrs(self):
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5, name="poy")
        r._ActiveRecord__origin_attrs = dict(name='ryan', age=10)
        r._sync_attrs()
        self.assertFalse(r.is_dirty())
        self.assertEquals('poy', r._get_origin('name'))
        self.assertEquals(None, r._get_origin('age'))

    def test_id_not_in_columns_should_raise_exception(self):
        try:
            class Foo(ActiveRecord):
                __columns__ = ['name', 'age']
            raise Exception('Cannot process this line.')
        except PKColumnNotInColumns, ex:
            pass
        
        
if __name__ == '__main__':
    unittest.main()
