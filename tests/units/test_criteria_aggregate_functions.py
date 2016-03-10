#coding: utf8
from sweet.criteria import Criteria
import unittest
import fudge


class CriteriaAggregateFunctionTestCase(unittest.TestCase):
    
    def get_criteria(self, conn=None):
        return Criteria(conn)

    def test_count_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT COUNT(*) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, criteria.count() )
        self.assertIsNone(criteria._aggregate)
    
    def test_sum_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT SUM(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, criteria.sum('age') )
        self.assertIsNone(criteria._aggregate)

    def test_avg_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT AVG(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, criteria.avg('age') )
        self.assertIsNone(criteria._aggregate)
        
    def test_min_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT MIN(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, criteria.min('age') )
        self.assertIsNone(criteria._aggregate)
        
    def test_max_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT MAX(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        criteria = self.get_criteria(conn)
        criteria.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, criteria.max('age') )
        self.assertIsNone(criteria._aggregate)

if __name__ == "__main__":
    unittest.main()
