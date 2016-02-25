# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from datetime import datetime 
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['name', 'age']


class RecordInsertTestCase(unittest.TestCase):

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_save_should_be_process_insert_if_record_has_not_been_persisted(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('insert').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord(name='foo').save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
     
    @fudge.patch('pyactive.record.ar.Criteria')
    def test_save_should_be_process_update_if_record_has_been_persisted(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').returns_fake()\
                .expects('update').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord(id=1, name='foo')
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
 
    @fudge.patch('pyactive.record.ar.Criteria')
    def test_create(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('insert').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord.create(name='foo')
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_save_for_insert_should_auto_builder_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo')
        r.__columns__ = ['name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertTrue(isinstance(r.updated_at, datetime))

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_save_for_update_should_auto_builder_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').returns_fake()\
                .expects('update').returns(1)
        r = OrmRecord(id=1, name='foo')
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.__columns__ = ['name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
        
 
if __name__ == '__main__':
    unittest.main()
