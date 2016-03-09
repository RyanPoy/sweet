# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
from sweet.utils import RecordNotFound
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'name', 'age']
    __created_at__ = __updated_at__ = None


class RecordQueryTestCase(unittest.TestCase):
    
    def setUp(self):
        self.tbname = OrmRecord.__table_name__

    def tearDown(self):
        OrmRecord.__table_name__ = self.tbname 
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_all(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('all').returns([{'id': 1, 'name':'poy', 'age':100}])
        self.assertEqual([{'id': 1, 'name':'poy', 'age':100}], OrmRecord.all())

    @fudge.patch('sweet.record.ar.Criteria')
    def test_first(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('first').returns({'id': 1, 'name':'poy', 'age':100})
        self.assertEqual({'id': 1, 'name':'poy', 'age':100}, OrmRecord.first())

    @fudge.patch('sweet.record.ar.Criteria')
    def test_last(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('last').returns({'id': 1, 'name':'poy', 'age':100})
        self.assertEqual({'id': 1, 'name':'poy', 'age':100}, OrmRecord.last())

    @fudge.patch('sweet.record.ar.Criteria')
    def test_count(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('count').returns(10)
        self.assertEqual(10, OrmRecord.count())
     
    @fudge.patch('sweet.record.ar.Criteria')
    def test_sum(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('sum').with_args('age').returns(100)
        self.assertEqual(100, OrmRecord.sum('age'))

    @fudge.patch('sweet.record.ar.Criteria')
    def test_max(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('max').with_args('age').returns(50)
        self.assertEqual(50, OrmRecord.max('age'))
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_min(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('min').with_args('age').returns(10)
        self.assertEqual(10, OrmRecord.min('age'))

    @fudge.patch('sweet.record.ar.Criteria')
    def test_avg(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('avg').with_args('age').returns(20)
        self.assertEqual(20, OrmRecord.avg('age'))
    
    @fudge.patch('sweet.record.ar.Criteria')
    def test_distinct(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('distinct')
        OrmRecord.distinct()
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_where(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(age=100).returns_fake()\
                .expects('where').with_args('name = ?', 10)
        OrmRecord.where(age=100).where('name = ?', 10)
    
    @fudge.patch('sweet.record.ar.Criteria')
    def test_having(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(age=100).returns_fake()\
                .expects('having').with_args('name = ?', 10)
        OrmRecord.where(age=100).having('name = ?', 10)
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_limit(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('limit').with_args(10)
        OrmRecord.limit(10)
     
    @fudge.patch('sweet.record.ar.Criteria')
    def test_offset(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('offset').with_args(20)
        OrmRecord.offset(20)
    
    @fudge.patch('sweet.record.ar.Criteria')
    def test_page(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('page').with_args(20, 50)
        OrmRecord.page(20, 50)
    
    @fudge.patch('sweet.record.ar.Criteria')
    def test_group_by(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('group_by').with_args('age')
        OrmRecord.group_by('age')
    
    @fudge.patch('sweet.record.ar.Criteria')
    def test_order_by(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('order_by').with_args('age DESC')
        OrmRecord.order_by('age DESC')

    @fudge.patch('sweet.record.ar.Criteria')
    def test_find_by_id_should_raise_exception_if_id_does_not_found(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=(1, )).returns_fake()\
                .expects('first').returns(None)
        self.assertRaises(RecordNotFound, OrmRecord.find, 1)
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_find_by_id_should_one(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=(1, )).returns_fake()\
                .expects('first').returns({"id":1, "name":'poy', 'age':10})
        self.assertEqual({"id":1, "name":'poy', 'age':10}, OrmRecord.find(1))
        
    @fudge.patch('sweet.record.ar.Criteria')
    def test_find_by_ids_should_one(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('where').with_args(id=(1, 2)).returns_fake()\
                .expects('all').returns([{"id":1, "name":'poy', 'age':10}, {"id":2, "name":'ryan', 'age':20}])
        self.assertEqual([{"id":1, "name":'poy', 'age':10}, {"id":2, "name":'ryan', 'age':20}], OrmRecord.find(1, 2))

    @fudge.patch('sweet.record.ar.Criteria')
    def test_select(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('select').with_args('id', 'name')
        OrmRecord.select('id', 'name')

    @fudge.patch('sweet.record.ar.Criteria')
    def test_join(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('join').with_args('users', 'users.id = ?', 10)
        OrmRecord.join('users', 'users.id = ?', 10)

    @fudge.patch('sweet.record.ar.Criteria')
    def test_left_join(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('left_join').with_args('users', 'users.id = ?', 10)
        OrmRecord.left_join('users', 'users.id = ?', 10)

    @fudge.patch('sweet.record.ar.Criteria')
    def test_right_join(self, Criteria):
        Criteria.is_callable().returns_fake()\
                .expects('from_').returns_fake()\
                .expects('right_join').with_args('users', 'users.id = ?', 10)
        OrmRecord.right_join('users', 'users.id = ?', 10)


if __name__ == '__main__':
    unittest.main()
