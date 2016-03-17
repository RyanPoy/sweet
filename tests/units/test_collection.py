#coding: utf8
import unittest
import fudge
from sweet.collection import Collection
from sweet.record import ActiveRecord
from sweet.relation import has_many
from sweet.criteria import Criteria
from datetime import datetime


class CollectionTest(unittest.TestCase):

    def test_all_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34} ]
        self.assertEqual([ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34} ], c.all())
 
    @fudge.patch('sweet.record.Criteria')
    def test_all_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('all').returns([
                     Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                     Phone(id=2, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                     Phone(id=3, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                 ])
        self.assertTrue(isinstance(u.phones, Collection))
        phones = u.phones.all()
        self.assertEqual(3, len(phones))
        self.assertEqual(1, phones[0].id)
        self.assertEqual(2, phones[1].id)
        self.assertEqual(3, phones[2].id)
        self.assertEqual(phones, u.phones._cache)

    def test_first_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34} ]
        self.assertEqual({'name': 'py', 'age': 35}, c.first())

    @fudge.patch('sweet.record.Criteria')
    def test_first_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('first').returns(
                    Phone(id=1, created_at=datetime.now(), updated_at=datetime.now(), user_id=10)
                )
        self.assertTrue(isinstance(u.phones, Collection))
        phone = u.phones.first()
        self.assertEqual(1, phone.id)
        self.assertEqual(10, phone.user_id)
        
    def test_last_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34} ]
        self.assertEqual({'name': 'ryan', 'age': 34}, c.last())

    @fudge.patch('sweet.record.Criteria')
    def test_last_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('last').returns(
                    Phone(id=3, created_at=datetime.now(), updated_at=datetime.now(), user_id=10)
                )
        self.assertTrue(isinstance(u.phones, Collection))
        phone = u.phones.last()
        self.assertEqual(3, phone.id)
        self.assertEqual(10, phone.user_id)

    def test_empty_collection_is_empty(self):
        c = Collection(None, None)
        c._cache = []
        self.assertTrue(c.is_empty())

    def test_offset_access_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 36} ]
        self.assertEqual({'name': 'ryan', 'age': 34}, c[1])

    @fudge.patch('sweet.record.Criteria')
    def test_offset_access_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('limit').with_args(1).returns_fake()\
                .expects('offset').with_args(1).returns_fake()\
                .expects('first').returns(
                     Phone(id=2, created_at=datetime.now(), updated_at=datetime.now(), user_id=10),
                 )
        self.assertTrue(isinstance(u.phones, Collection))
        phones = u.phones
        self.assertEqual(2, phones[1].id)
        self.assertIsNone(u.phones._cache)

    def test_avg_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 37} ]
        self.assertEqual(35.333333333333336, c.avg('age'))

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
        c._cache = [ User({'name': 'py', 'age': 35}), {'name': 'ryan', 'age': 34}]
        self.assertEqual(34.5, c.avg('age'))

    @fudge.patch('sweet.record.Criteria')
    def test_avg_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('avg').with_args('age').returns(24.75)
        self.assertEqual(24.75, u.phones.avg('age'))

    def test_count_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 37} ]
        self.assertEqual(3, c.count())

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
        c._cache = [ User({'name': 'py', 'age': 35}), {'name': 'ryan', 'age': 34}]
        self.assertEqual(2, c.count())

    @fudge.patch('sweet.record.Criteria')
    def test_count_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('count').with_args('*').returns(10)
        self.assertEqual(10, u.phones.count())

    def test_sum_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 37} ]
        self.assertEqual(106, c.sum('age'))

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
        c._cache = [ User({'name': 'py', 'age': 35}), {'name': 'ryan', 'age': 34}]
        self.assertEqual(69, c.sum('age'))

    @fudge.patch('sweet.record.Criteria')
    def test_sum_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('sum').with_args('age').returns(69)
        self.assertEqual(69, u.phones.sum('age'))

    def test_min_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 37} ]
        self.assertEqual(34, c.min('age'))

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
        c._cache = [ User({'name': 'py', 'age': 35}), {'name': 'ryan', 'age': 34}]
        self.assertEqual(34, c.min('age'))

    @fudge.patch('sweet.record.Criteria')
    def test_min_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('min').with_args('age').returns(34)
        self.assertEqual(34, u.phones.min('age'))

    def test_max_from_cache(self):
        c = Collection(relation=None, owner_instance=None)
        c._cache = [ {'name': 'py', 'age': 35}, {'name': 'ryan', 'age': 34},  {'name': 'poy', 'age': 37} ]
        self.assertEqual(37, c.max('age'))

        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
        c._cache = [ User({'name': 'py', 'age': 35}), {'name': 'ryan', 'age': 34}]
        self.assertEqual(35, c.max('age'))

    @fudge.patch('sweet.record.Criteria')
    def test_max_from_db_of_has_many_relation(self, Criteria):
        class Phone(ActiveRecord):
            __columns__ = ['id', 'created_at', 'updated_at', 'user_id']
            __dbmanager__ = fudge.Fake('dbmanager').provides('get_connection').returns(None)
 
        class User(ActiveRecord):
            __columns__ = ['id', 'name', 'created_at', 'updated_at']
            has_many(Phone)
 
        u = User(id=10, name='py', age=35)
        u._ActiveRecord__is_persisted = True
        
        Criteria.is_callable().returns_fake()\
                .expects('set_record_class').with_args(Phone).returns_fake()\
                .expects('where').with_args(user_id=10).returns_fake()\
                .expects('max').with_args('age').returns(37)
        self.assertEqual(37, u.phones.max('age'))

 #    def test_each(self):
 #        original = ['foo', 'bar', 'baz']
 #        c = Collection(original)

 #        result = []
 #        c.each(lambda x: result.append(x))
 #        self.assertEqual(result, original)
 #        self.assertEqual(original, c.all())

 #    def test_every(self):
 #        c = Collection([1, 2, 3, 4, 5, 6])
 #        self.assertEqual([1, 3, 5], c.every(2).all())
 #        self.assertEqual([2, 4, 6], c.every(2, 1).all())

 #    def test_filter(self):
 #        c = Collection([{'id': 1, 'name': 'hello'}, {'id': 2, 'name': 'world'}])
 #        self.assertEqual([{'id': 2, 'name': 'world'}], c.filter(lambda item: item['id'] == 2).all())

 #        c = Collection(['', 'hello', '', 'world'])
 #        self.assertEqual(['hello', 'world'], c.filter().all())

 #    def test_where(self):
 #        c = Collection([{'v': 1}, {'v': 3}, {'v': 2}, {'v': 3}, {'v': 4}])
 #        self.assertEqual([{'v': 3}, {'v': 3}], c.where('v', 3).all())

 #    def test_flip(self):
 #        c = Collection({'foo': 'bar', 'baz': 'boom'})
 #        self.assertEqual({'bar': 'foo', 'boom': 'baz'}, c.flip().all())

 #    def test_implode(self):
 #        obj1 = type('lamdbaobject', (object,), {})()
 #        obj1.name = 'john'
 #        obj1.email = 'foo'
 #        c = Collection([{'name': 'john', 'email': 'foo'}, {'name': 'jane', 'email': 'bar'}])
 #        self.assertEqual('foobar', c.implode('email'))
 #        self.assertEqual('foo,bar', c.implode('email', ','))

 #        c = Collection(['foo', 'bar'])
 #        self.assertEqual('foobar', c.implode(''))
 #        self.assertEqual('foo,bar', c.implode(','))

 #    def test_lists(self):
 #        obj1 = type('lamdbaobject', (object,), {})()
 #        obj1.name = 'john'
 #        obj1.email = 'foo'
 #        c = Collection([obj1, {'name': 'jane', 'email': 'bar'}])
 #        self.assertEqual({'john': 'foo', 'jane': 'bar'}, c.lists('email', 'name').all())
 #        self.assertEqual(['foo', 'bar'], c.pluck('email').all())

 #    def test_map(self):
 #        c = Collection([1, 2, 3, 4, 5])
 #        self.assertEqual([3, 4, 5, 6, 7], c.map(lambda x: x + 2).all())

 #    def test_merge(self):
 #        c = Collection([1, 2, 3])
 #        c.merge([4, 5, 6])
 #        self.assertEqual([1, 2, 3, 4, 5, 6], c.all())

 #        c = Collection(Collection([1, 2, 3]))
 #        c.merge([4, 5, 6])
 #        self.assertEqual([1, 2, 3, 4, 5, 6], c.all())

 #        c = Collection({'foo': 'bar'})
 #        c.merge({'baz': 'boom'})
 #        self.assertEqual({'foo': 'bar', 'baz': 'boom'}, c.all())

 #    def test_for_page(self):
 #        c = Collection([1, 2, 3, 4, 5, 6])
 #        self.assertEqual([4, 5, 6], c.for_page(2, 3).all())
 #        self.assertEqual([5, 6], c.for_page(2, 4).all())

 #    def test_prepend(self):
 #        c = Collection([4, 5, 6])
 #        c.prepend(3)
 #        self.assertEqual([3, 4, 5, 6], c.all())

 #    def test_append(self):
 #        c = Collection([3, 4, 5])
 #        c.append(6)
 #        self.assertEqual([3, 4, 5, 6], c.all())

 #    def test_pull(self):
 #        c = Collection([1, 2, 3, 4])
 #        c.pull(2)
 #        self.assertEqual([1, 2, 4], c.all())

 #        c = Collection({'foo': 'bar', 'baz': 'boom'})
 #        c.pull('baz')
 #        self.assertEqual({'foo': 'bar'}, c.all())

 #    def test_put(self):
 #        c = Collection([1, 2, 4])
 #        c.put(2, 3)
 #        self.assertEqual([1, 2, 3], c.all())

 #        c = Collection({'foo': 'bar'})
 #        c.put('baz', 'boom')
 #        self.assertEqual({'foo': 'bar', 'baz': 'boom'}, c.all())

 #    def test_reject(self):
 #        c = Collection([1, 2, 3, 4, 5, 6])
 #        self.assertEqual([1, 2, 3], c.reject(lambda x: x > 3).all())

 #    def test_reverse(self):
 #        c = Collection([1, 2, 3, 4])
 #        self.assertEqual([4, 3, 2, 1], c.reverse().all())

 #    def test_sort(self):
 #        c = Collection([5, 3, 1, 2, 4])

 #        sorted = c.sort(lambda x: x)
 #        self.assertEqual([1, 2, 3, 4, 5], sorted.all())

 #    def test_take(self):
 #        c = Collection([1, 2, 3, 4, 5, 6])
 #        self.assertEqual([1, 2, 3], c.take(3).all())
 #        self.assertEqual([4, 5, 6], c.take(-3).all())

 #    def test_transform(self):
 #        c = Collection([1, 2, 3, 4])
 #        c.transform(lambda x: x + 2)
 #        self.assertEqual([3, 4, 5, 6], c.all())

 #    def test_zip(self):
 #        c = Collection([1, 2, 3])
 #        self.assertEqual([(1, 4), (2, 5), (3, 6)], c.zip([4, 5, 6]).all())

 #    def test_only(self):
 #        c = Collection([1, 2, 3, 4, 5])
 #        self.assertEqual([2, 4], c.only(1, 3).all())

 #        c = Collection({'foo': 'bar', 'baz': 'boom'})
 #        self.assertEqual({'baz': 'boom'}, c.only('baz').all())

 #    def test_without(self):
 #        c = Collection([1, 2, 3, 4, 5])
 #        self.assertEqual([1, 3, 5], c.without(1, 3).all())
 #        self.assertEqual([1, 2, 3, 4, 5], c.all())

 #        c = Collection({'foo': 'bar', 'baz': 'boom'})
 #        self.assertEqual({'baz': 'boom'}, c.without('foo').all())
 #        self.assertEqual({'foo': 'bar', 'baz': 'boom'}, c.all())

 #    def test_flatten(self):
 #        c = Collection({'foo': [5, 6], 'bar': 7, 'baz': {'boom': [1, 2, 3, 4]}})

 #        self.assertEqual(
 #            [1, 2, 3, 4, 5, 6, 7],
 #            c.flatten().sort().all()
 #        )

 #        c = Collection([1, [2, 3], 4])
 #        self.assertEqual([1, 2, 3, 4], c.flatten().all())


if __name__ == '__main__':
    unittest.main()