# -*- coding: utf-8 -*-
from sweet.record import ActiveRecord
import unittest
import fudge


class OrmRecord(ActiveRecord):
    __columns__ = ['id', 'created_at', 'updated_at']


class RecordTestCase(unittest.TestCase):
    
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
        r = OrmRecord({'a':1, 'b':2}, c=3, d=4, b=5)
        self.assertEquals(1, r.a)
        self.assertEquals(3, r.c)
        self.assertEquals(4, r.d)
        self.assertEquals(5, r.b)

        self.assertEquals(1, r.cur_value('a'))
        self.assertEquals(5, r.cur_value('b'))
        self.assertEquals(3, r.cur_value('c'))
        self.assertEquals(4, r.cur_value('d'))
        
        self.assertEquals(1, r.last_value('a'))
        self.assertEquals(5, r.last_value('b'))
        self.assertEquals(3, r.last_value('c'))
        self.assertEquals(4, r.last_value('d'))
        
    def test_dirty_attributes(self):
        r = OrmRecord(a='1', b=2, c=3)
        r.a = 1
        r.b = 20
        
        self.assertEquals(1, r.cur_value('a'))
        self.assertEquals(20, r.cur_value('b'))
        self.assertEquals(3, r.last_value('c'))

        self.assertEquals('1', r.last_value('a'))
        self.assertEquals(2, r.last_value('b'))
        self.assertEquals(3, r.last_value('c'))
        
        self.assertTrue(r.is_dirty())
        self.assertTrue(r.is_dirty('a'))
        self.assertTrue(r.is_dirty('b'))
        self.assertFalse(r.is_dirty('c'))
        self.assertFalse(r.is_dirty('a', 'b', 'c'))

 
#     def test_hydrate_creates_collection_of_models(self):
#         data = [
#             {'name': 'john'},
#             {'name': 'jane'}
#         ]
#         collection = OrmRecord.hydrate(data, 'foo_connection')
# 
#         self.assertIsInstance(collection, Collection)
#         self.assertEqual(2, len(collection))
#         self.assertIsInstance(collection[0], OrmRecord)
#         self.assertIsInstance(collection[1], OrmRecord)
#         self.assertEqual(collection[0].get_attributes(), collection[0].get_original())
#         self.assertEqual(collection[1].get_attributes(), collection[1].get_original())
#         self.assertEqual('john', collection[0].name)
#         self.assertEqual('jane', collection[1].name)
#         self.assertEqual('foo_connection', collection[0].get_connection_name())
#         self.assertEqual('foo_connection', collection[1].get_connection_name())
# 
#     def test_hydrate_raw_makes_raw_query(self):
#         model = OrmModelHydrateRawStub()
#         connection = MockConnection().prepare_mock()
#         connection.select.return_value = []
#         r.get_connection = mock.MagicMock(return_value=connection)
# 
#         def _set_connection(name):
#             r.__connection__ = name
# 
#             return model
# 
#         OrmModelHydrateRawStub.set_connection = mock.MagicMock(side_effect=_set_connection)
#         collection = OrmModelHydrateRawStub.hydrate_raw('SELECT ?', ['foo'])
#         self.assertEqual('hydrated', collection)
#         connection.select.assert_called_once_with(
#             'SELECT ?', ['foo']
#         )
# 
#     def test_create_saves_new_model(self):
#         model = OrmModelSaveStub.create(name='john')
#         self.assertTrue(r.get_saved())
#         self.assertEqual('john', r.name)
# 
#     def test_find_method_calls_query_builder_correctly(self):
#         result = OrmModelFindStub.find(1)
# 
#         self.assertEqual('foo', result)
# 
#     def test_find_use_write_connection(self):
#         OrmModelFindWithWriteConnectionStub.on_write_connection().find(1)
# 
#     def test_find_with_list_calls_query_builder_correctly(self):
#         result = OrmModelFindManyStub.find([1, 2])
# 
#         self.assertEqual('foo', result)
# 
#     def test_destroy_method_calls_query_builder_correctly(self):
#         OrmModelDestroyStub.destroy(1, 2, 3)
# 
#     def test_with_calls_query_builder_correctly(self):
#         result = OrmModelWithStub.with_('foo', 'bar')
#         self.assertEqual('foo', result)
# 
#     def test_update_process(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'name': 'john'})
# 
#         model = OrmRecord()
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updating: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updated: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('saved: %s' % r.__class__.__name__, model)\
#             .and_return(True)
# 
#         r.id = 1
#         r.foo = 'bar'
#         r.sync_original()
#         r.name = 'john'
#         r.set_exists(True)
#         self.assertTrue(r.save())
# 
#         r.new_query.assert_called_once_with()
#         r._update_timestamps.assert_called_once_with()
# 
#     def test_update_process_does_not_override_timestamps(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'created_at': 'foo', 'updated_at': 'bar'})
# 
#         model = OrmRecord()
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
# 
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updating: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updated: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('saved: %s' % r.__class__.__name__, model)\
#             .and_return(True)
# 
#         r.id = 1
#         r.sync_original()
#         r.created_at = 'foo'
#         r.updated_at = 'bar'
#         r.set_exists(True)
#         self.assertTrue(r.save())
# 
#         r.new_query.assert_called_once_with()
#         self.assertTrue(r._update_timestamps.called)
# 
#     def test_creating_with_only_created_at_column(self):
#         query_builder = flexmock(QueryBuilder)
#         query_builder.should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
# 
#         model = flexmock(OrmModelCreatedAt())
#         r.should_receive('new_query').and_return(Builder(QueryBuilder(None, None, None)))
#         r.should_receive('set_created_at').once()
#         r.should_receive('set_updated_at').never()
#         r.name = 'john'
#         r.save()
# 
#     def test_creating_with_only_updated_at_column(self):
#         query_builder = flexmock(QueryBuilder)
#         query_builder.should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
# 
#         model = flexmock(OrmModelUpdatedAt())
#         r.should_receive('new_query').and_return(Builder(QueryBuilder(None, None, None)))
#         r.should_receive('set_created_at').never()
#         r.should_receive('set_updated_at').once()
#         r.name = 'john'
#         r.save()
# 
#     def test_updating_with_only_created_at_column(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'name': 'john'})
# 
#         model = flexmock(OrmModelCreatedAt())
#         r.id = 1
#         r.sync_original()
#         r.set_exists(True)
#         r.should_receive('new_query').and_return(Builder(QueryBuilder(None, None, None)))
#         r.should_receive('set_created_at').never()
#         r.should_receive('set_updated_at').never()
#         r.name = 'john'
#         r.save()
# 
#     def test_updating_with_only_updated_at_column(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'name': 'john'})
# 
#         model = flexmock(OrmModelUpdatedAt())
#         r.id = 1
#         r.sync_original()
#         r.set_exists(True)
#         r.should_receive('new_query').and_return(Builder(QueryBuilder(None, None, None)))
#         r.should_receive('set_created_at').never()
#         r.should_receive('set_updated_at').once()
#         r.name = 'john'
#         r.save()
# 
#     def test_update_is_cancelled_if_updating_event_returns_false(self):
#         model = flexmock(OrmRecord())
#         query = flexmock(Builder(flexmock(QueryBuilder(None, None, None))))
#         r.should_receive('new_query_without_scopes').once().and_return(query)
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updating: %s' % r.__class__.__name__, model)\
#             .and_return(False)
#         r.set_exists(True)
#         r.foo = 'bar'
# 
#         self.assertFalse(r.save())
# 
#     def test_update_process_without_timestamps(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'name': 'john'})
# 
#         model = flexmock(OrmRecord())
#         r.__timestamps__ = False
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
# 
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         r.should_receive('_fire_model_event').and_return(True)
# 
#         r.id = 1
#         r.sync_original()
#         r.name = 'john'
#         r.set_exists(True)
#         self.assertTrue(r.save())
# 
#         r.new_query.assert_called_once_with()
#         self.assertFalse(r._update_timestamps.called)
# 
#     def test_update_process_uses_old_primary_key(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'id': 2, 'name': 'john'})
# 
#         model = OrmRecord()
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
# 
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updating: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('updated: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('saved: %s' % r.__class__.__name__, model)\
#             .and_return(True)
# 
#         r.id = 1
#         r.sync_original()
#         r.id = 2
#         r.name = 'john'
#         r.set_exists(True)
#         self.assertTrue(r.save())
# 
#         r.new_query.assert_called_once_with()
#         self.assertTrue(r._update_timestamps.called)
# 
#     def test_timestamps_are_returned_as_objects(self):
#         model = Model()
#         r.set_raw_attributes({
#             'created_at': '2015-03-24',
#             'updated_at': '2015-03-24'
#         })
# 
#         self.assertIsInstance(r.created_at, Arrow)
#         self.assertIsInstance(r.updated_at, Arrow)
# 
#     def test_timestamps_are_returned_as_objects_from_timestamps_and_datetime(self):
#         model = Model()
#         r.set_raw_attributes({
#             'created_at': datetime.datetime.utcnow(),
#             'updated_at': time.time()
#         })
# 
#         self.assertIsInstance(r.created_at, Arrow)
#         self.assertIsInstance(r.updated_at, Arrow)
# 
#     def test_timestamps_are_returned_as_objects_on_create(self):
#         model = Model()
#         r.unguard()
# 
#         timestamps = {
#             'created_at': datetime.datetime.now(),
#             'updated_at': datetime.datetime.now()
#         }
# 
#         instance = r.new_instance(timestamps)
# 
#         self.assertIsInstance(instance.created_at, Arrow)
#         self.assertIsInstance(instance.updated_at, Arrow)
# 
#         r.reguard()
# 
#     def test_timestamps_return_none_if_set_to_none(self):
#         model = Model()
#         r.unguard()
# 
#         timestamps = {
#             'created_at': datetime.datetime.now(),
#             'updated_at': datetime.datetime.now()
#         }
# 
#         instance = r.new_instance(timestamps)
#         instance.created_at = None
# 
#         self.assertIsNone(instance.created_at)
# 
#         r.reguard()
# 
#     def test_insert_process(self):
#         query = flexmock(Builder)
# 
#         model = OrmRecord()
#         query_builder = flexmock(QueryBuilder)
#         query_builder.should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
# 
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('creating: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('created: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('saved: %s' % r.__class__.__name__, model)\
#             .and_return(True)
# 
#         r.name = 'john'
#         r.set_exists(False)
#         self.assertTrue(r.save())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
#         self.assertTrue(r._update_timestamps.called)
# 
#         model = OrmRecord()
#         query_builder.should_receive('insert').once().with_args({'name': 'john'})
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
#         r._update_timestamps = mock.MagicMock()
#         r.set_incrementing(False)
# 
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('creating: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('created: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('saved: %s' % r.__class__.__name__, model)\
#             .and_return(True)
# 
#         r.name = 'john'
#         r.set_exists(False)
#         self.assertTrue(r.save())
#         self.assertFalse(hasattr(model, 'id'))
#         self.assertTrue(r.exists)
#         self.assertTrue(r._update_timestamps.called)
# 
#     def test_insert_is_cancelled_if_creating_event_returns_false(self):
#         model = flexmock(OrmRecord())
#         query = flexmock(Builder(flexmock(QueryBuilder(None, None, None))))
#         r.should_receive('new_query_without_scopes').once().and_return(query)
#         events = flexmock(Event())
#         r.__dispatcher__ = events
#         events.should_receive('fire').once()\
#             .with_args('saving: %s' % r.__class__.__name__, model)\
#             .and_return(True)
#         events.should_receive('fire').once()\
#             .with_args('creating: %s' % r.__class__.__name__, model)\
#             .and_return(False)
# 
#         self.assertFalse(r.save())
#         self.assertFalse(r.exists)
# 
#     def test_delete_properly_deletes_model(self):
#         model = OrmRecord()
#         builder = flexmock(Builder(QueryBuilder(None, None, None)))
#         builder.should_receive('where').once().with_args('id', 1).and_return(builder)
#         builder.should_receive('delete').once()
#         r.new_query = mock.MagicMock(return_value=builder)
#         r.touch_owners = mock.MagicMock()
# 
#         r.set_exists(True)
#         r.id = 1
#         r.delete()
# 
#         self.assertTrue(r.touch_owners.called)
# 
#     def test_push_no_relations(self):
#         model = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.should_receive('new_query').once().and_return(builder)
#         r.should_receive('_update_timestamps').once()
# 
#         r.name = 'john'
#         r.set_exists(False)
# 
#         self.assertTrue(r.push())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
# 
#     def test_push_empty_one_relation(self):
#         model = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.should_receive('new_query').once().and_return(builder)
#         r.should_receive('_update_timestamps').once()
# 
#         r.name = 'john'
#         r.set_exists(False)
#         r.set_relation('relation_one', None)
# 
#         self.assertTrue(r.push())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
#         self.assertIsNone(r.relation_one)
# 
#     def test_push_one_relation(self):
#         related1 = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'related1'}, 'id').and_return(2)
#         related1.should_receive('new_query').once().and_return(builder)
#         related1.should_receive('_update_timestamps').once()
# 
#         related1.name = 'related1'
#         related1.set_exists(False)
# 
#         model = flexmock(Model())
#         r.should_receive('resolve_connection').and_return(MockConnection().prepare_mock())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.should_receive('new_query').once().and_return(builder)
#         r.should_receive('_update_timestamps').once()
# 
#         r.name = 'john'
#         r.set_exists(False)
#         r.set_relation('relation_one', related1)
# 
#         self.assertTrue(r.push())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
#         self.assertEqual(2, r.relation_one.id)
#         self.assertTrue(r.relation_one.exists)
#         self.assertEqual(2, related1.id)
#         self.assertTrue(related1.exists)
# 
#     def test_push_empty_many_relation(self):
#         model = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.should_receive('new_query').once().and_return(builder)
#         r.should_receive('_update_timestamps').once()
# 
#         r.name = 'john'
#         r.set_exists(False)
#         r.set_relation('relation_many', Collection([]))
# 
#         self.assertTrue(r.push())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
#         self.assertEqual(0, len(r.relation_many))
# 
#     def test_push_many_relation(self):
#         related1 = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'related1'}, 'id').and_return(2)
#         related1.should_receive('new_query').once().and_return(builder)
#         related1.should_receive('_update_timestamps').once()
# 
#         related1.name = 'related1'
#         related1.set_exists(False)
# 
#         related2 = flexmock(Model())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'related2'}, 'id').and_return(3)
#         related2.should_receive('new_query').once().and_return(builder)
#         related2.should_receive('_update_timestamps').once()
# 
#         related2.name = 'related2'
#         related2.set_exists(False)
# 
#         model = flexmock(Model())
#         r.should_receive('resolve_connection').and_return(MockConnection().prepare_mock())
#         query = flexmock(QueryBuilder(MockConnection().prepare_mock(), QueryGrammar(), QueryProcessor()))
#         builder = Builder(query)
#         builder.get_query().should_receive('insert_get_id').once().with_args({'name': 'john'}, 'id').and_return(1)
#         r.should_receive('new_query').once().and_return(builder)
#         r.should_receive('_update_timestamps').once()
# 
#         r.name = 'john'
#         r.set_exists(False)
#         r.set_relation('relation_many', Collection([related1, related2]))
# 
#         self.assertTrue(r.push())
#         self.assertEqual(1, r.id)
#         self.assertTrue(r.exists)
#         self.assertEqual(2, len(r.relation_many))
#         self.assertEqual([2, 3], r.relation_many.lists('id'))
# 
#     def test_new_query_returns_orator_query_builder(self):
#         conn = flexmock(Connection)
#         grammar = flexmock(QueryGrammar)
#         processor = flexmock(QueryProcessor)
#         conn.should_receive('get_query_grammar').and_return(grammar)
#         conn.should_receive('get_post_processor').and_return(processor)
#         resolver = flexmock(DatabaseManager)
#         resolver.should_receive('connection').and_return(Connection(None))
#         OrmRecord.set_connection_resolver(DatabaseManager({}))
# 
#         model = OrmRecord()
#         builder = r.new_query()
#         self.assertIsInstance(builder, Builder)
# 
#     def test_get_key_returns_primary_key_value(self):
#         model = OrmRecord()
#         r.id = 1
#         self.assertEqual(1, r.get_key())
#         self.assertEqual('id', r.get_key_name())
# 
#     def test_connection_management(self):
#         resolver = flexmock(DatabaseManager)
#         resolver.should_receive('connection').once().with_args('foo').and_return('bar')
# 
#         OrmRecord.set_connection_resolver(DatabaseManager({}))
#         model = OrmRecord()
#         r.set_connection('foo')
# 
#         self.assertEqual('bar', r.get_connection())
# 
#     def test_to_dict(self):
#         model = OrmRecord()
#         r.name = 'foo'
#         r.age = None
#         r.password = 'password1'
#         r.set_hidden(['password'])
#         r.set_relation('names', Collection([OrmRecord(bar='baz'), OrmRecord(bam='boom')]))
#         r.set_relation('partner', OrmRecord(name='jane'))
#         r.set_relation('group', None)
#         r.set_relation('multi', Collection())
# 
#         d = r.to_dict()
# 
#         self.assertIsInstance(d, dict)
#         self.assertEqual('foo', d['name'])
#         self.assertEqual('baz', d['names'][0]['bar'])
#         self.assertEqual('boom', d['names'][1]['bam'])
#         self.assertEqual('jane', d['partner']['name'])
#         self.assertIsNone(d['group'])
#         self.assertEqual([], d['multi'])
#         self.assertIsNone(d['age'])
#         self.assertNotIn('password', d)
# 
#         r.set_appends(['appendable'])
#         d = r.to_dict()
#         self.assertEqual('appended', d['appendable'])
# 
#     def test_to_dict_includes_default_formatted_timestamps(self):
#         model = Model()
#         r.set_raw_attributes({
#             'created_at': '2015-03-24',
#             'updated_at': '2015-03-25'
#         })
# 
#         d = r.to_dict()
# 
#         self.assertEqual('2015-03-24T00:00:00+00:00', d['created_at'])
#         self.assertEqual('2015-03-25T00:00:00+00:00', d['updated_at'])
# 
#     def test_to_dict_includes_custom_formatted_timestamps(self):
#         class Stub(Model):
# 
#             def get_date_format(self):
#                 return 'DD-MM-YY'
# 
#         flexmock(Stub).should_receive('_boot_columns').and_return(['created_at', 'updated_at'])
# 
#         model = Stub()
#         r.set_raw_attributes({
#             'created_at': '2015-03-24',
#             'updated_at': '2015-03-25'
#         })
# 
#         d = r.to_dict()
# 
#         self.assertEqual('24-03-15', d['created_at'])
#         self.assertEqual('25-03-15', d['updated_at'])
# 
#     def test_visible_creates_dict_whitelist(self):
#         model = OrmRecord()
#         r.set_visible(['name'])
#         r.name = 'John'
#         r.age = 28
#         d = r.to_dict()
# 
#         self.assertEqual({'name': 'John'}, d)
# 
#     def test_hidden_can_also_exclude_relationships(self):
#         model = OrmRecord()
#         r.name = 'John'
#         r.set_relation('foo', ['bar'])
#         r.set_hidden(['foo', 'list_items', 'password'])
#         d = r.to_dict()
# 
#         self.assertEqual({'name': 'John'}, d)
# 
#     def test_to_dict_uses_mutators(self):
#         model = OrmRecord()
#         r.list_items = [1, 2, 3]
#         d = r.to_dict()
# 
#         self.assertEqual([1, 2, 3], d['list_items'])
# 
#         model = OrmRecord(list_items=[1, 2, 3])
#         d = r.to_dict()
# 
#         self.assertEqual([1, 2, 3], d['list_items'])
# 
#     def test_hidden_are_ignored_when_visible(self):
#         model = OrmRecord(name='john', age=28, id='foo')
#         r.set_visible(['name', 'id'])
#         r.set_hidden(['name', 'age'])
#         d = r.to_dict()
# 
#         self.assertIn('name', d)
#         self.assertIn('id', d)
#         self.assertNotIn('age', d)
# 
#     def test_fillable(self):
#         model = OrmRecord()
#         r.fillable(['name', 'age'])
#         r.fill(name='foo', age=28)
#         self.assertEqual('foo', r.name)
#         self.assertEqual(28, r.age)
# 
#     def test_fill_with_dict(self):
#         model = OrmRecord()
#         r.fill({'name': 'foo', 'age': 28})
#         self.assertEqual('foo', r.name)
#         self.assertEqual(28, r.age)
# 
#     def test_unguard_allows_anything(self):
#         model = OrmRecord()
#         r.unguard()
#         r.guard(['*'])
#         r.fill(name='foo', age=28)
#         self.assertEqual('foo', r.name)
#         self.assertEqual(28, r.age)
#         r.reguard()
# 
#     def test_underscore_properties_are_not_filled(self):
#         model = OrmRecord()
#         r.fill(_foo='bar')
#         self.assertEqual({}, r.get_attributes())
# 
#     def test_guarded(self):
#         model = OrmRecord()
#         r.guard(['name', 'age'])
#         r.fill(name='foo', age='bar', foo='bar')
#         self.assertFalse(hasattr(model, 'name'))
#         self.assertFalse(hasattr(model, 'age'))
#         self.assertEqual('bar', r.foo)
# 
#     def test_fillable_overrides_guarded(self):
#         model = OrmRecord()
#         r.guard(['name', 'age'])
#         r.fillable(['age', 'foo'])
#         r.fill(name='foo', age='bar', foo='bar')
#         self.assertFalse(hasattr(model, 'name'))
#         self.assertEqual('bar', r.age)
#         self.assertEqual('bar', r.foo)
# 
#     def test_global_guarded(self):
#         model = OrmRecord()
#         r.guard(['*'])
#         self.assertRaises(
#             MassAssignmentError,
#             r.fill,
#             name='foo', age='bar', foo='bar'
#         )
# 
#     # TODO: test relation
# 
#     def test_models_assumes_their_name(self):
#         model = OrmModelNoTableStub()
# 
#         self.assertEqual('orm_model_no_table_stubs', r.get_table())
# 
#     def test_mutator_cache_is_populated(self):
#         model = OrmRecord()
# 
#         expected_attributes = sorted([
#             'list_items',
#             'password',
#             'appendable'
#         ])
# 
#         self.assertEqual(expected_attributes, sorted(list(r._get_mutated_attributes().keys())))
# 
#     def test_fresh_method(self):
#         model = flexmock(OrmRecord())
#         r.id = 1
#         r.set_exists(True)
#         flexmock(Builder)
#         q = flexmock(QueryBuilder(None, None, None))
#         query = flexmock(Builder(q))
#         query.should_receive('where').and_return(query)
#         query.get_query().should_receive('take').and_return(query)
#         query.should_receive('get').and_return(Collection([]))
#         r.should_receive('with_').once().with_args('foo', 'bar').and_return(query)
# 
#         r.fresh(['foo', 'bar'])
# 
#         r.should_receive('with_').once().with_args().and_return(query)
# 
#         r.fresh()
# 
#     def test_clone_model_makes_a_fresh_copy(self):
#         model = OrmRecord()
#         r.id = 1
#         r.set_exists(True)
#         r.first = 'john'
#         r.last = 'doe'
#         r.created_at = r.fresh_timestamp()
#         r.updated_at = r.fresh_timestamp()
#         # TODO: relation
# 
#         clone = r.replicate()
# 
#         self.assertFalse(hasattr(clone, 'id'))
#         self.assertFalse(clone.exists)
#         self.assertEqual('john', clone.first)
#         self.assertEqual('doe', clone.last)
#         self.assertFalse(hasattr(clone, 'created_at'))
#         self.assertFalse(hasattr(clone, 'updated_at'))
#         # TODO: relation
# 
#         clone.first = 'jane'
# 
#         self.assertEqual('john', r.first)
#         self.assertEqual('jane', clone.first)
# 
#     def test_get_attribute_raise_attribute_error(self):
#         model = OrmRecord()
# 
#         try:
#             relation = r.incorrect_relation
#             self.fail('AttributeError not raised')
#         except AttributeError:
#             pass
# 
#     def test_increment(self):
#         query = flexmock()
#         model_mock = flexmock(OrmRecord, new_query=lambda: query)
#         model = OrmRecord()
#         r.set_exists(True)
#         r.id = 1
#         r.sync_original_attribute('id')
#         r.foo = 2
# 
#         model_mock.should_receive('new_query').and_return(query)
#         query.should_receive('where').and_return(query)
#         query.should_receive('increment')
# 
#         r.public_increment('foo')
# 
#         self.assertEqual(3, r.foo)
#         self.assertFalse(r.is_dirty())
# 
#     # TODO: relationship touch_owners is propagated
# 
#     # TODO: relationship touch_owners is not propagated if no relationship result
# 
#     def test_timestamps_are_not_update_with_timestamps_false_save_option(self):
#         query = flexmock(Builder)
#         query.should_receive('where').once().with_args('id', 1)
#         query.should_receive('update').once().with_args({'name': 'john'})
# 
#         model = OrmRecord()
#         r.new_query = mock.MagicMock(return_value=Builder(QueryBuilder(None, None, None)))
# 
#         r.id = 1
#         r.sync_original()
#         r.name = 'john'
#         r.set_exists(True)
#         self.assertTrue(r.save({'timestamps': False}))
#         self.assertFalse(hasattr(model, 'updated_at'))
# 
#         r.new_query.assert_called_once_with()
# 
#     def test_casts(self):
#         model = OrmModelCastingStub()
#         r.first = '3'
#         r.second = '4.0'
#         r.third = 2.5
#         r.fourth = 1
#         r.fifth = 0
#         r.sixth = {'foo': 'bar'}
#         r.seventh = ['foo', 'bar']
#         r.eighth = {'foo': 'bar'}
# 
#         self.assertIsInstance(r.first, int)
#         self.assertIsInstance(r.second, float)
#         self.assertIsInstance(r.third, basestring)
#         self.assertIsInstance(r.fourth, bool)
#         self.assertIsInstance(r.fifth, bool)
#         self.assertIsInstance(r.sixth, dict)
#         self.assertIsInstance(r.seventh, list)
#         self.assertIsInstance(r.eighth, dict)
#         self.assertTrue(r.fourth)
#         self.assertFalse(r.fifth)
#         self.assertEqual({'foo': 'bar'}, r.sixth)
#         self.assertEqual({'foo': 'bar'}, r.eighth)
#         self.assertEqual(['foo', 'bar'], r.seventh)
# 
#         d = r.to_dict()
# 
#         self.assertIsInstance(d['first'], int)
#         self.assertIsInstance(d['second'], float)
#         self.assertIsInstance(d['third'], basestring)
#         self.assertIsInstance(d['fourth'], bool)
#         self.assertIsInstance(d['fifth'], bool)
#         self.assertIsInstance(d['sixth'], dict)
#         self.assertIsInstance(d['seventh'], list)
#         self.assertIsInstance(d['eighth'], dict)
#         self.assertTrue(d['fourth'])
#         self.assertFalse(d['fifth'])
#         self.assertEqual({'foo': 'bar'}, d['sixth'])
#         self.assertEqual({'foo': 'bar'}, d['eighth'])
#         self.assertEqual(['foo', 'bar'], d['seventh'])
# 
#     def test_casts_preserve_null(self):
#         model = OrmModelCastingStub()
#         r.first = None
#         r.second = None
#         r.third = None
#         r.fourth = None
#         r.fifth = None
#         r.sixth = None
#         r.seventh = None
#         r.eighth = None
# 
#         self.assertIsNone(r.first)
#         self.assertIsNone(r.second)
#         self.assertIsNone(r.third)
#         self.assertIsNone(r.fourth)
#         self.assertIsNone(r.fifth)
#         self.assertIsNone(r.sixth)
#         self.assertIsNone(r.seventh)
#         self.assertIsNone(r.eighth)
# 
#         d = r.to_dict()
# 
#         self.assertIsNone(d['first'])
#         self.assertIsNone(d['second'])
#         self.assertIsNone(d['third'])
#         self.assertIsNone(d['fourth'])
#         self.assertIsNone(d['fifth'])
#         self.assertIsNone(d['sixth'])
#         self.assertIsNone(d['seventh'])
#         self.assertIsNone(d['eighth'])
# 
#     def test_get_foreign_key(self):
#         model = OrmRecord()
#         r.set_table('stub')
# 
#         self.assertEqual('stub_id', r.get_foreign_key())
# 
#     def test_default_values(self):
#         model = OrmModelDefaultAttributes()
# 
#         self.assertEqual('bar', r.foo)
# 
# 
# class OrmModelHydrateRawStub(Model):
# 
#     @classmethod
#     def hydrate(cls, items, connection=None):
#         return 'hydrated'
# 
# 
# class OrmModelWithStub(Model):
# 
#     def new_query(self):
#         mock = flexmock(Builder(None))
#         mock.should_receive('with_').once().with_args('foo', 'bar').and_return('foo')
# 
#         return mock
# 
# 
# class OrmModelSaveStub(Model):
# 
#     __table__ = 'save_stub'
# 
#     __guarded__ = []
# 
#     def save(self, options=None):
#         self.__saved = True
# 
#     def set_incrementing(self, value):
#         self.__incrementing__ = value
# 
#     def get_saved(self):
#         return self.__saved
# 
# 
# class OrmModelFindStub(Model):
# 
#     def new_query(self):
#         flexmock(Builder).should_receive('find').once().with_args(1, ['*']).and_return('foo')
# 
#         return Builder(None)
# 
# 
# class OrmModelFindWithWriteConnectionStub(Model):
# 
#     def new_query(self):
#         mock = flexmock(Builder)
#         mock_query = flexmock(QueryBuilder)
#         mock_query.should_receive('use_write_connection').once().and_return(flexmock)
#         mock.should_receive('find').once().with_args(1).and_return('foo')
# 
#         return Builder(QueryBuilder(None, None, None))
# 
# 
# class OrmModelFindManyStub(Model):
# 
#     def new_query(self):
#         mock = flexmock(Builder)
#         mock.should_receive('find').once().with_args([1, 2], ['*']).and_return('foo')
# 
#         return Builder(QueryBuilder(None, None, None))
# 
# 
# class OrmModelDestroyStub(Model):
# 
#     def new_query(self):
#         mock = flexmock(Builder)
#         model = flexmock()
#         mock_query = flexmock(QueryBuilder)
#         mock_query.should_receive('where_in').once().with_args('id', [1, 2, 3]).and_return(flexmock)
#         mock.should_receive('get').once().and_return([model])
#         r.should_receive('delete').once()
# 
#         return Builder(QueryBuilder(None, None, None))
# 
# 
# class OrmModelNoTableStub(Model):
# 
#     pass
# 
# 
# class OrmModelCastingStub(Model):
# 
#     __casts__ = {
#         'first': 'int',
#         'second': 'float',
#         'third': 'str',
#         'fourth': 'bool',
#         'fifth': 'boolean',
#         'sixth': 'dict',
#         'seventh': 'list',
#         'eighth': 'json'
#     }
# 
# class OrmModelCreatedAt(Model):
# 
#     __timestamps__ = ['created_at']
# 
# 
# class OrmModelUpdatedAt(Model):
# 
#     __timestamps__ = ['updated_at']
# 
# 
# class OrmModelDefaultAttributes(Model):
# 
#     __attributes__ = {
#         'foo': 'bar'
#     }
#
