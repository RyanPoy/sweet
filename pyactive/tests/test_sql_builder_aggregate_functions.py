#coding: utf8
from pyactive.query.sql_builder import SQLBuilder
import unittest
import fudge


class SQLBuilderAggregateFunctionTestCase(unittest.TestCase):
    
    def get_builder(self, conn=None):
        return SQLBuilder(conn)

    def test_count_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT COUNT(*) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, builder.count() )
        self.assertIsNone(builder._aggregate)
    
    def test_sum_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT SUM(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, builder.sum('age') )
        self.assertIsNone(builder._aggregate)

    def test_avg_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT AVG(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, builder.avg('age') )
        self.assertIsNone(builder._aggregate)
        
    def test_min_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT MIN(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, builder.min('age') )
        self.assertIsNone(builder._aggregate)
        
    def test_max_aggegate_function(self):
        results = [{'aggregate': 20}]
        conn = fudge.Fake('conn')\
                .expects('fetch_all')\
                .with_args('SELECT MAX(age) AS aggregate FROM `users` WHERE `users`.`tag` IN (?, ?) LIMIT 1', 'boom', 'foo')\
                .returns(results)
        builder = self.get_builder(conn)
        builder.select('*').from_('users').where(tag=['boom', 'foo'])
        self.assertEqual(20, builder.max('age') )
        self.assertIsNone(builder._aggregate)

if __name__ == "__main__":
    unittest.main()
