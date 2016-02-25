# -*- coding: utf-8 -*-
from ..record import ActiveRecord
import unittest


class OrmRecord(ActiveRecord):
    __columns__ = ['name', 'age']


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
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5)
        self.assertEquals(1, r.a)
        self.assertEquals(3, r.c)
        self.assertEquals(4, r.d)
        self.assertEquals(5, r.b)
        
    def test_persisit_attrs(self):
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5, name="poy")
        self.assertEqual({'name': "poy", 'age': None}, r.persist_attrs)
    
    
if __name__ == '__main__':
    unittest.main()
