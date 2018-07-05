# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.utils import *
from datetime import datetime, date 
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']
    __created_at__ = __updated_at__ = None
    __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)


class RecordUpdateTestCase(unittest.TestCase):

    @fudge.patch('sweet.record.Criteria')
    def test_update(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('set_record_class').with_args(OrmRecord).returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').with_args({'name': 'foo'}).returns(2)
        r = OrmRecord(id=1)
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        relt = r.update_attributes(name='foo')
        self.assertTrue(relt)
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)

    def test_update_should_raise_exception_if_record_has_not_been_persisted(self):
        r = OrmRecord()
        self.assertRaises(RecordHasNotBeenPersisted, r.update_attributes, name='foo')
        self.assertIsNone(r.name)
 
    @fudge.patch('sweet.record.Criteria')
    def test_save_should_auto_builder_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('set_record_class').with_args(OrmRecord).returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').returns(2)
        r = OrmRecord(id=1)
        r.__columns__ = ['name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.update_attributes(name='foo')
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
    
    @fudge.patch('sweet.record.Criteria')
    def test_save_with_customized_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('set_record_class').with_args(OrmRecord).returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').returns(2)
        r = OrmRecord(id=1)
        r.__columns__ = ['id', 'name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.name = 'foo'
        r.updated_at = '2010-10-10 10:1:1'
        r.created_at = '2010-1-1 2:3:4'
        r.save()
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertEqual('2010-10-10 10:01:01', datetime2str(r.updated_at))
        self.assertEqual('2010-01-01 02:03:04', datetime2str(r.created_at))

    @fudge.patch('sweet.record.Criteria')
    def test_update_created_at_and_updated_at_cusmer(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('set_record_class').with_args(OrmRecord).returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').returns(2)
        r = OrmRecord(id=1)
        r.__columns__ = ['name', 'age', 'created_at', 'updated_at']
        r.__created_at__ = 'created_at'
        r.__updated_at__ = 'updated_at'
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        r.update_attributes(name='foo', updated_at='2010-10-10 10:1:1', created_at='2010-1-1 2:3:4')
        self.assertTrue(r.is_persisted)
        self.assertTrue(isinstance(r, OrmRecord))
        self.assertEqual('foo', r.name)
        self.assertEqual(1, r.id)
        self.assertTrue(isinstance(r.updated_at, datetime))
        self.assertTrue(isinstance(r.created_at, datetime))
        self.assertEqual('2010-10-10 10:01:01', datetime2str(r.updated_at))
        self.assertEqual('2010-01-01 02:03:04', datetime2str(r.created_at))


if __name__ == '__main__':
    unittest.main()

