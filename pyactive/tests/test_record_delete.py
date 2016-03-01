# -*- coding: utf-8 -*-
from ..record import ActiveRecord
from ..utils import RecordHasNotBeenPersisted
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']
    __created_at__ = __updated_at__ = None


class RecordDeleteTestCase(unittest.TestCase):
    
    def setUp(self):
        self.tbname = OrmRecord.__table_name__

    def tearDown(self):
        OrmRecord.__table_name__ = self.tbname
        
    def test_delete_should_raise_exception_if_record_has_not_been_persisted(self):
        r = OrmRecord()
        self.assertRaises(RecordHasNotBeenPersisted, r.delete)
        
    @fudge.patch('pyactive.record.ar.Criteria')
    def test_delete(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(**{'id': 10}).returns_fake()\
                .expects('delete').returns(1)
        r = OrmRecord(name='foo', id=10)
        r._ActiveRecord__is_persisted = True # 设置r是持久化状态
        self.assertEqual(1, r.delete())

    @fudge.patch('pyactive.record.ar.Criteria')
    def test_delete_all(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('delete').returns(10)
        self.assertEqual(10, OrmRecord.delete_all())


if __name__ == '__main__':
    unittest.main()
