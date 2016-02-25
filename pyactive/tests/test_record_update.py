# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from ..utils import RecordHasNotBeenPersisted
from datetime import datetime 
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['name', 'age']


class RecordUpdateTestCase(unittest.TestCase):

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_update(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('from_').returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').with_args({'name': 'foo'}).returns(1)
        r = OrmRecord(id=1)
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        relt = r.update_attributes(name='foo')
        self.assertTrue(relt)
        self.assertEqual('foo', r.name)

    def test_update_should_raise_exception_if_record_has_not_been_persisted(self):
        r = OrmRecord()
        self.assertRaises(RecordHasNotBeenPersisted, r.update_attributes, name='foo')
        self.assertIsNone(r.name)

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_save_should_auto_builder_created_at_and_updated_at(self, Criteria):
        Criteria.is_callable().returns_fake()\
            .expects('from_').returns_fake()\
            .expects('where').returns_fake()\
            .expects('update').returns(1)
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


if __name__ == '__main__':
    unittest.main()
