# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.utils import PKColumnNotInColumns
from datetime import datetime, date
import unittest


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']
    __created_at__ = __updated_at__ = None


class RecordBaseTestCase(unittest.TestCase):
    
    def setUp(self):
        self.tbname = OrmRecord.__table_name__

    def tearDown(self):
        OrmRecord.__table_name__ = self.tbname 
        
    def test_table_name(self):
        self.assertEqual('orm_records', OrmRecord.table_name) 
        self.assertEqual('orm_records', OrmRecord.__table_name__)
        OrmRecord.__table_name__ = 'record'
        self.assertEqual('record', OrmRecord.table_name) 
        
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
        
    def test_prepare_at(self):
        r = OrmRecord(id=1)
        r.__columns__ = ['name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        attrs_dict = {}
        r._prepare_at_or_on(attrs_dict)
        self.assertTrue(isinstance(attrs_dict['created_at'], datetime))
        self.assertTrue(isinstance(attrs_dict['updated_at'], datetime))
        self.assertTrue('created_on' not in r.__dict__)
        self.assertTrue('updated_on' not in r.__dict__)

    def test_prepare_on(self):
        r = OrmRecord(id=1)
        r.__columns__ = ['name', 'age', 'created_on', 'updated_on']
        r.__created_on__ = 'created_on'
        r.__updated_on__ = 'updated_on'
        attrs_dict = {}
        r._prepare_at_or_on(attrs_dict)
        self.assertTrue(isinstance(attrs_dict['created_on'], date))
        self.assertTrue(isinstance(attrs_dict['updated_on'], date))
        self.assertTrue('created_at' not in r.__dict__)
        self.assertTrue('updated_at' not in r.__dict__)

    def test_to_dict(self):
        r = OrmRecord(id=1)
        r.__columns__ = ['name', 'age', 'created_on', 'updated_at']
        r.__created_on__ = 'created_on'
        r.__updated_at__ = 'updated_at'
        r.name = 'py'
        r.age = 100
        d = r.to_dict()
        self.assertEquals('py', d.get('name'))
        self.assertEquals(100, d.get('age'))
        self.assertTrue(isinstance(d['created_on'], date))
        self.assertTrue(isinstance(d['updated_at'], datetime))


if __name__ == '__main__':
    unittest.main()
