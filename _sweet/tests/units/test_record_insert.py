# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.utils import datetime2str, date2str
from datetime import datetime, date 
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']
    __created_at__ = __updated_at__ = None
    __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)


class RecordInsertTestCase(unittest.TestCase):

    @fudge.patch('sweet.record.Criteria')
    def test_save_should_be_process_insert_if_record_has_not_been_persisted(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord(name='foo').save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)

        self.assertEqual('foo', r._get_origin('name'))
        self.assertEqual(1, r._get_origin('id'))
     
    @fudge.patch('sweet.record.Criteria')
    def test_save_should_be_process_update_if_record_has_been_persisted(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('where').returns_fake()\
                .expects('update').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord(id=1, name='foo')
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)

        self.assertEqual('foo', r._get_origin('name'))
        self.assertEqual(1, r._get_origin('id'))
  
    @fudge.patch('sweet.record.Criteria')
    def test_create(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').with_args(**{'name': 'foo', 'age':None}).returns(1)
        r = OrmRecord.create(name='foo')
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertEqual('foo', r._get_origin('name'))
        self.assertEqual(1, r._get_origin('id'))

    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_should_auto_builder_created_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo')
        r.__columns__ = ['id', 'name', 'age', 'created_at']
        r.__created_at__ = 'created_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_at, datetime))
        
    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_should_auto_builder_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo')
        r.__columns__ = ['id', 'name', 'age', 'updated_at']
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
        
    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_should_auto_builder_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo')
        r.__columns__ = ['id', 'name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertTrue(isinstance(r.updated_at, datetime))
        
    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_with_customized_created_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo', created_at="2015-10-1 10:09:10")
        r.__columns__ = ['id', 'name', 'age', 'created_at']
        r.__created_at__ = 'created_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertEqual('2015-10-01 10:09:10', datetime2str(r.created_at))

    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_with_customized_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo', updated_at="2015-10-1 10:9:10")
        r.__columns__ = ['id', 'name', 'age', 'updated_at']
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
        self.assertEqual('2015-10-01 10:09:10', datetime2str(r.updated_at))

    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_with_customized_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo', created_at="2015-10-1 10:09:10", updated_at="2016-1-1 1:2:3")
        r.__columns__ = ['id', 'name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertTrue(isinstance(r.updated_at, datetime))
        self.assertEqual('2015-10-01 10:09:10', datetime2str(r.created_at))
        self.assertEqual('2016-01-01 01:02:03', datetime2str(r.updated_at))
        
    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_should_auto_builder_created_on_and_updated_on(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo')
        r.__columns__ = ['id', 'name', 'age', 'created_on', 'updated_on']
        r.__created_on__ = 'created_on'
        r.__updated_on__ = 'updated_on'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_on, date))
        self.assertTrue(isinstance(r.updated_on, date))

    @fudge.patch('sweet.record.Criteria')
    def test_save_for_insert_with_customized_created_on_and_updated_on(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(OrmRecord).returns_fake()\
                .expects('insert').returns(1)
        r = OrmRecord(name='foo', created_on="2015-10-1", updated_on="2016-1-1")
        r.__columns__ = ['id', 'name', 'age', 'created_on', 'updated_on']
        r.__created_on__ = 'created_on'
        r.__updated_on__ = 'updated_on'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.created_on, date))
        self.assertTrue(isinstance(r.updated_on, date))
        self.assertEqual('2015-10-01', date2str(r.created_on))
        self.assertEqual('2016-01-01', date2str(r.updated_on))


if __name__ == '__main__':
    unittest.main()
