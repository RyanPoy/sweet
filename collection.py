# -*- coding: utf-8 -*-
from copy import copy
from sweet.record import ActiveRecord
import json


class Collection(object):

    def __init__(self, owner_instance, relation):
        self.owner_instance = owner_instance
        self.relation = relation
        self._cache = None

    def all(self):
        if self._cache is None:
            self._cache = self.relation.prefetch(self.owner_instance).all()
        return self._cache

    def first(self):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).first()
        return self._cache[0] if self._cache else None

    def last(self):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).last()
        return self._cache[-1] if self._cache else None
 
    def count(self, column='*'):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).count(column)
        return len(self._cache)

    def sum(self, column):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).sum(column)
        total, cnt = self._total_and_cnt_of_cache(column)
        return total

    def avg(self, column):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).avg(column)
        total, cnt = self._total_and_cnt_of_cache(column)
        return 0 if cnt == 0 else total*1.0 / cnt

    def _total_and_cnt_of_cache(self, column):
        total, cnt = 0, 0
        for item in self._cache:
            tp = type(item)
            if tp == dict:
                v = item.get(column, None)
            elif issubclass(tp, ActiveRecord):
                v = getattr(item, column, None)
            else:
                raise Exception('Collection#avg can not support %s type item' % tp)
            if v is not None: 
                total += long(v)
                cnt += 1
        return total, cnt

    def max(self, column):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).max(column)
        _min, _max = self._min_and_max_of_cache(column)
        return _max

    def min(self, column):
        if self._cache is None:
            return self.relation.prefetch(self.owner_instance).min(column)
        _min, _max = self._min_and_max_of_cache(column)
        return _min

    def _min_and_max_of_cache(self, column):
        set_v, _min, _max = False, 0, 0
        for item in self._cache:
            tp = type(item)
            if tp == dict:
                v = item.get(column, None)
            elif issubclass(tp, ActiveRecord):
                v = getattr(item, column, None)
            else:
                raise Exception('Collection#avg can not support %s type item' % tp)
            if v is not None:
                v = long(v)
                if not set_v:
                    _min = _max = v
                    set_v = True
                else:
                    if _min > v: _min = v
                    if _max < v: _max = v
        return _min, _max

#     @classmethod
#     def min(cls, column):
#         return cls._new_criteria().min(column)
    
#     @classmethod  
#     def distinct(cls):
#         return cls._new_criteria().distinct()
    
#     @classmethod
#     def where(cls, *args, **kwargs):
#         """ condition query
#         eg.
#             User.where(username="abc").where(password="123")
#             User.where("username='abc' and password='123'")
#             User.where("username=? and password=?", 'abc', '123')
#         """
#         return cls._new_criteria().where(*args, **kwargs)
    
#     @classmethod
#     def find(cls, *ids):
#         """ find record by ids
#         @return:    if there is a id and found it will return a record
#                     if there are many ids and found them will return a record array
#                     if any id not found, throw RecordNotFound exception
#         """
#         c = cls._new_criteria().where(id=ids)
#         if len(ids) == 1:
#             relt = c.first()
#             if relt is None:
#                 raise RecordNotFound()
#         else:
#             relt = c.all()
#         return relt
 
#     @classmethod
#     def limit(cls, limit):
#         """ limit query
#         eg. 
#             User.limit(10)
#             User.limit(10, 1)
#         """
#         return cls._new_criteria().limit(limit)
    
#     @classmethod
#     def offset(cls, offset):
#         """ limit query
#         eg. 
#             User.limit(10)
#             User.limit(10, 1)
#         """
#         return cls._new_criteria().offset(offset)
    
#     @classmethod
#     def page(cls, page_num, limit):
#         return cls._new_criteria().page(page_num, limit)
    
#     @classmethod
#     def group_by(cls, *args):
#         """ group query
#         eg. 
#             User.group_by('username')
#         """
#         return cls._new_criteria().group_by(*args)
    
#     @classmethod
#     def order_by(cls, *args):
#         """ order query
#         eg.
#             User.order_by('age')
#             User.order_by('age ASC')
#             User.order_by('age DESC')
#         """
#         return cls._new_criteria().order_by(*args)
    
#     @classmethod
#     def having(cls, *args, **kwargs):
#         """ having query when group
#         Note: if there is not use group, the having will be not useful
#         eg.
#             User.group('username').having(age=1)
#         """
#         return cls._new_criteria().having(*args, **kwargs)
    
#     @classmethod
#     def select(cls, *select_columns):
#         return cls._new_criteria().select(*select_columns)
    
#     @classmethod
#     def join(cls, tablename, *args):
#         return cls._new_criteria().join(tablename, *args)
    
#     @classmethod
#     def left_join(cls, tablename, *args):
#         return cls._new_criteria().left_join(tablename, *args)
    
#     @classmethod
#     def right_join(cls, tablename, *args):
#         return cls._new_criteria().right_join(tablename, *args)

#     def valid(self):
#         return True

#     def _get_attr(self, attrname, default=None):
#         return getattr(self, attrname, default)

#     @classmethod
#     def create(cls, attr_dict={}, **attributes):
#         """ persist a active record. 
#         @return：record instance if successful, else return None. 
#                 it will throw exception when any exception occur
#         eg. 
#             User.create(username='abc', password='123')
#         """
#         record = cls(attr_dict, **attributes)
#         return record if record.save() else None
    
#     def  _prepare_at_or_on(self, attrs_dict):
#         def _build_at_or_on(at_or_on_attrname, attrs_dict, is_at=True):
#             if not at_or_on_attrname: return
#             str_2_datetime_or_date = str2datetime if is_at else str2date
#             cur_datetime_or_date = datetime.now if is_at else datetime.today
#             if at_or_on_attrname:
#                 value = attrs_dict.get(at_or_on_attrname, None) or self._get_attr(at_or_on_attrname)
#                 value = str_2_datetime_or_date(value)
#                 if value is None:
#                     value = cur_datetime_or_date() 
#                 setattr(self, at_or_on_attrname, value)
#                 attrs_dict[at_or_on_attrname] = self._get_attr(at_or_on_attrname)
#         for at in (self.__created_at__, self.__updated_at__):
#             _build_at_or_on(at, attrs_dict)
#         for on in (self.__created_on__, self.__updated_on__):
#             _build_at_or_on(on, attrs_dict, False)
#         return self

#     def save(self):
#         """ persist a record instance.
#         @return：record instace if successful, else return None. 
#                 it will throw exception when any exception occur
#         eg.
#             u = User(username="abc", password="123456").save()
#         """
#         criteria = self._new_criteria()
#         if self.is_persisted: # update
#             attrs = self.persist_attrs()
#             self._prepare_at_or_on(attrs)
#             criteria.where(**{self.__pk__:self.pk}).update(**attrs)
#         else: # insert
#             attrs = self.persist_attrs()
#             self._prepare_at_or_on(attrs)
#             setattr(self, self.__pk__, criteria.insert(**attrs))
#             self.__is_persisted = True
#         self._sync_attrs()
#         return self

#     @classmethod
#     def _new_criteria(cls):
#         return Criteria(cls.__dbmanager__.get_connection()).set_record_class(cls)

# #     def relate(self, *models):
# #         Collection(self.__class__).save(self)
# #         return self

#     def update_attributes(self, **attributes):
#         """ update attributes
#         eg. 
#             u = User(username="abc", password="123").save().update_attributes(username="efg", password="456")
#         return: self if update successful else False
#         """
#         if not self.is_persisted:
#             raise RecordHasNotBeenPersisted()
#         self._prepare_at_or_on(attributes)
#         criteria = self._new_criteria().where(**{self.__pk__:self.pk})
#         criteria.update(attributes)

#         for name, value in attributes.iteritems():
#             setattr(self, name, value)
#         return self

#     @classmethod
#     def update_all(cls, **attributes):
#         cls._new_criteria().update(**attributes)
#         return True

#     @property
#     def pk(self):
#         return getattr(self, self.__pk__)

#     def delete(self):
#         """delete a record instance.
#         eg.
#             u = User.find(1)
#             u.delete()
#         """
#         if not self.is_persisted:
#             raise RecordHasNotBeenPersisted()
        
# #         for r in self.__relations__:
# #             r._delete(self)

#         # @TODO: 需要按照关系再次删除相关的数据
#         c = self._new_criteria()
#         return c.where(**{self.__pk__:self.pk}).delete()

#     @classmethod
#     def delete_all(cls):
#         """ delete all records
#         eg.
#             User.delete_all()
#             User.find(1, 2, 3).delete_all()
#         """
#         c = cls._new_criteria()
#         return c.delete()

#     @classmethod
#     def _get_db(cls):
#         if cls.__db__ is None:
#             cls.__db__ = pyrails.get_database()
#         return cls.__db__
#     
#     @classproperty
#     @contextmanager
#     def transaction(cls):
#         try:
#             database = cls._get_db()
#             database.set_autocommit(False)
#             yield None
#             database.commit()
#         except ValidationError:
#             database.rollback()
#         except:
#             database.rollback()
#             raise
#         finally:
#             try:
#                 database.set_autocommit(True)
#             except:
#                 pass
# 
#     def add_error(self, attr_name, msg):
#         self.errors.setdefault(attr_name, []).append(msg)
#         return self
# 
#     def validate(self, on='save'):
#         return all([ valid(record=self) for valid in self.__class__.validate_func_dict.get(on, {}) ])
#
    
    # @classproperty
    # def columns(cls):
    #     return cls.__columns__

    # @classmethod
    # def has_column(cls, column_name):
    #     return column_name in cls.__columns__

    # def to_dict(self, contain_relations=False):
    #     d = dict([ (c, getattr(self, c)) for c in self.__columns__ ])
    #     self._prepare_at_or_on(d)
    #     if contain_relations:
    #         pass
    #     return d

    # def __init__(self, items=None):
    #     """
    #     Creates a new Collection

    #     :param items: The collection items
    #     :type items: dict or list or Collection or map

    #     :rtype: None
    #     """
    #     if items is None:
    #         items = []
    #     else:
    #         items = self._get_items(items)

    #     if not isinstance(items, (list, dict, tuple)):
    #         self._items = [items]
    #     else:
    #         self._items = items

    # @property
    # def items(self):
    #     return self._items

    # @classmethod
    # def make(cls, items=None):
    #     """
    #     Create a new Collection instance if the value isn't one already

    #     :param items: The collection items
    #     :type items: dict or list or Collection

    #     :return: A Collection instance
    #     :rtype: Collection
    #     """
    #     if isinstance(items, Collection):
    #         return items

    #     return cls(items)

    # def all(self):
    #     """
    #     Get all of the items in the collection.

    #     :return: The items in the collections
    #     :type: mixed
    #     """
    #     return self._cache

    # def avg(self, key=None):
    #     """
    #     Get the average value of a given key.

    #     :param key: The key to get the average for
    #     :type key: mixed

    #     :rtype: float or int
    #     """
    #     count = self.count()

    #     if count:
    #         return self.sum(key) / count

    # def chunk(self, size, preserve_keys=False):
    #     """
    #     Chunk the underlying collection.

    #     :param size: The chunk size
    #     :type size: int

    #     :param preserve_keys: Preserve the dictionary keys
    #     :type preserve_keys: bool

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         if not preserve_keys:
    #             items = list(self._cache.values())
    #         else:
    #             chunks = []
    #             chunk = {}
    #             for i, key in enumerate(self._cache.keys()):
    #                 if i != 0 and i % size == 0:
    #                     chunks.append(chunk)
    #                     chunk = {}

    #                 chunk[key] = self._cache[key]

    #             return self.__class__(list(map(Collection, chunks)))
    #     else:
    #         items = self._cache

    #     chunks = [items[i:i + size] for i in range(0, len(items), size)]

    #     return self.__class__(list(map(Collection, chunks)))

    # def collapse(self):
    #     """
    #     Collapse the collection items into a single element (dict or list)

    #     :return: A new Collection instance with collapsed items
    #     :rtype: Collection
    #     """
    #     results = []

    #     if isinstance(self._items, dict):
    #         items = self._cache.values()
    #     else:
    #         items = self._cache

    #     for values in items:
    #         if isinstance(values, Collection):
    #             values = values.all()

    #         results += values

    #     return self.__class__(results)

    # def contains(self, key, value=None):
    #     """
    #     Determine if an element is in the collection

    #     :param key: The element
    #     :type key: int or str or callable

    #     :param value: The value of the element
    #     :type value: mixed

    #     :return: Whether the element is in the collection
    #     :rtype: bool
    #     """
    #     if value is not None:
    #         return self.contains(lambda x: data_get(x, key) == value)

    #     if self._use_as_callable(key):
    #         return self.first(key) is not None

    #     return key in self._cache

    # def __contains__(self, item):
    #     return self.contains(item)

    # def count(self):
    #     return len(self._cache)

    # def diff(self, items):
    #     """
    #     Diff the collections with the given items

    #     :param items: The items to diff with
    #     :type items: mixed

    #     :return: A Collection instance
    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         elements = {}
    #         for key, val in self._cache.items():
    #             if key not in items:
    #                 elements[key] = val
    #             elif items[key] != val:
    #                 elements[key] = val

    #         return self.__class__(elements)
    #     else:
    #         return self.__class__([i for i in self._cache if i not in items])

    # def each(self, callback):
    #     """
    #     Execute a callback over each item.

    #     .. code::

    #         collection = Collection([1, 2, 3])
    #         collection.each(lambda x: x + 3)

    #     .. warning::

    #         It only applies the callback but does not modify the collection's items.
    #         Use the `transform() <#orator.support.Collection.transform>`_ method to
    #         modify the collection.

    #     :param callback: The callback to execute
    #     :type callback: callable

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         items = self._cache.values()
    #     else:
    #         items = self._cache

    #     for item in items:
    #         if callback(item) is False:
    #             break

    #     return self

    # def every(self, step, offset=0):
    #     """
    #     Create a new collection consisting of every n-th element.

    #     :param step: The step size
    #     :type step: int

    #     :param offset: The start offset
    #     :type offset: int

    #     :rtype: Collection
    #     """
    #     new = []

    #     for position, item in enumerate(self._cache):
    #         if position % step == offset:
    #             new.append(item)

    #     return self.__class__(new)

    # def without(self, *keys):
    #     """
    #     Get all items except for those with the specified keys.

    #     :param keys: The keys to remove
    #     :type keys: tuple

    #     :rtype: Collection
    #     """
    #     items = copy(self._cache)

    #     if not isinstance(items, dict):
    #         keys = reversed(sorted(keys))

    #     for key in keys:
    #         del items[key]

    #     return self.__class__(items)

    # def only(self, *keys):
    #     """
    #     Get the items with the specified keys.

    #     :param keys: The keys to keep
    #     :type keys: tuple

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         items = {}

    #         for key, value in self._cache.items():
    #             if key in keys:
    #                 items[key] = value
    #     else:
    #         items = []

    #         for key, value in enumerate(self._cache):
    #             if key in keys:
    #                 items.append(value)

    #     return self.__class__(items)

    # def filter(self, callback=None):
    #     """
    #     Run a filter over each of the items.

    #     :param callback: The filter callback
    #     :type callback: callable or None

    #     :rtype: Collection
    #     """
    #     if callback:
    #         return self.__class__(list(filter(callback, self._cache)))

    #     return self.__class__(list(filter(None, self._cache)))

    # def where(self, key, value):
    #     """
    #     Filter items by the given key value pair.

    #     :param key: The key to filter by
    #     :type key: str

    #     :param value: The value to filter by
    #     :type value: mixed

    #     :rtype: Collection
    #     """
    #     return self.filter(lambda item: data_get(item, key) == value)

    # def first(self, callback=None, default=None):
    #     """
    #     Get the first item of the collection.

    #     :param default: The default value
    #     :type default: mixed
    #     """
    #     if isinstance(self._cache, dict):
    #         raise CollectionError('first() method cannot be used on dictionary items')

    #     if callback is not None:
    #         for val in self._cache:
    #             if callback(val):
    #                 return val

    #         return value(default)

    #     if len(self._cache) > 0:
    #         return self._cache[0]
    #     else:
    #         return default

    # def flatten(self):
    #     """
    #     Get a flattened list of the items in the collection.

    #     :rtype: Collection
    #     """
    #     def _flatten(d):
    #         if isinstance(d, dict):
    #             for v in d.values():
    #                 for nested_v in _flatten(v):
    #                     yield nested_v
    #         elif isinstance(d, list):
    #             for list_v in d:
    #                 for nested_v in _flatten(list_v):
    #                     yield nested_v
    #         else:
    #             yield d

    #     return Collection(list(_flatten(self._cache)))

    # def flip(self):
    #     """
    #     Flip the items in the collection.

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         items = dict(zip(self._cache.values(), self._cache.keys()))
    #     else:
    #         items = self._cache

    #     return self.__class__(items)

    # def forget(self, *keys):
    #     """
    #     Remove an item from the collection by key.

    #     :param keys: The keys to remove
    #     :type keys: tuple

    #     :rtype: Collection
    #     """
    #     if not isinstance(self._cache, dict):
    #         keys = reversed(sorted(keys))

    #     for key in keys:
    #         del self[key]

    #     return self

    # def get(self, key, default=None):
    #     """
    #     Get an element of the collection.

    #     :param key: The index of the element
    #     :type key: mixed

    #     :param default: The default value to return
    #     :type default: mixed

    #     :rtype: mixed
    #     """
    #     if isinstance(self._cache, dict):
    #         if key in self:
    #             return self[key]

    #         return value(default)

    #     try:
    #         return self._cache[key]
    #     except IndexError:
    #         return value(default)

    # def implode(self, value, glue=''):
    #     """
    #     Concatenate values of a given key as a string.

    #     :param value: The value
    #     :type value: str

    #     :param glue: The glue
    #     :type glue: str

    #     :rtype: str
    #     """
    #     first = self.first()

    #     if not isinstance(first, (basestring)):
    #         return glue.join(self.pluck(value).all())

    #     return value.join(self._cache)

    # def last(self, callback=None, default=None):
    #     """
    #     Get the last item of the collection.

    #     :param default: The default value
    #     :type default: mixed
    #     """
    #     if isinstance(self._cache, dict):
    #         raise CollectionError('last() method cannot be used on dictionary items')

    #     if callback is not None:
    #         for val in reversed(self._cache):
    #             if callback(val):
    #                 return val

    #         return value(default)

    #     if len(self._cache) > 0:
    #         return self._cache[-1]
    #     else:
    #         return default

    # def pluck(self, value, key=None):
    #     """
    #     Get a list with the values of a given key.

    #     :rtype: Collection
    #     """
    #     if key:
    #         results = dict(map(lambda x: (data_get(x, key), data_get(x, value)), self._cache))
    #     else:
    #         results = list(map(lambda x: data_get(x, value), self._cache))

    #     return self.__class__(results)

    # def lists(self, value, key=None):
    #     """
    #     Alias for "pluck"

    #     :rtype: list
    #     """
    #     return self.pluck(value, key)

    # def map(self, callback):
    #     """
    #     Run a map over each of the item.

    #     :param callback: The map function
    #     :type callback: callable

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         return self.__class__(list(map(callback, self._cache.values())))

    #     return self.__class__(list(map(callback, self._cache)))

    # def max(self, key=None):
    #     """
    #     Get the max value of a given key.

    #     :param key: The key
    #     :type key: str or None

    #     :rtype: mixed
    #     """
    #     def _max(result, item):
    #         val = data_get(item, key)

    #         if result is None or val > result:
    #             return value

    #         return result

    #     return self.reduce(_max)

    # def min(self, key=None):
    #     """
    #     Get the min value of a given key.

    #     :param key: The key
    #     :type key: str or None

    #     :rtype: mixed
    #     """
    #     def _min(result, item):
    #         val = data_get(item, key)

    #         if result is None or val < result:
    #             return value

    #         return result

    #     return self.reduce(_min)

    # def merge(self, items):
    #     """
    #     Merge the collection with the given items.

    #     :param items: The items to merge
    #     :type items: list or dict or Collection

    #     :rtype: Collection
    #     """
    #     if isinstance(items, Collection):
    #         items = items.all()

    #     if isinstance(self._cache, dict) and not isinstance(items, dict) \
    #             or isinstance(self._cache, list) and not isinstance(items, list):
    #         raise CollectionError('Unable to merge uncompatible types')

    #     if isinstance(self._cache, dict):
    #         self._cache.update(items)
    #     else:
    #         self._items += items

    #     return self

    # def for_page(self, page, per_page):
    #     """
    #     "Paginate" the collection by slicing it into a smaller collection.

    #     :param page: The current page
    #     :type page: int

    #     :param per_page: Number of items by slice
    #     :type per_page: int

    #     :rtype: Collection
    #     """
    #     start = (page - 1) * per_page
    #     return self[start:start + per_page]

    # def pop(self, key=None):
    #     """
    #     Remove the item at the given index, and return it.
    #     If no index is specified, returns the last item.

    #     :param key: The index of the item to return
    #     :type key: mixed

    #     :rtype: mixed
    #     """
    #     if isinstance(self._cache, dict):
    #         value = self._cache[key]

    #         del self._cache[key]

    #         return value

    #     if key is None:
    #         key = -1

    #     return self._cache.pop(key)

    # def prepend(self, value):
    #     """
    #     Push an item onto the beginning of the collection.

    #     :param value: The value to push
    #     :type value: mixed

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         return self

    #     self._cache.insert(0, value)

    #     return self

    # def append(self, value):
    #     """
    #     Push an item onto the end of the collection.

    #     :param value: The value to push
    #     :type value: mixed

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         return self

    #     self._cache.append(value)

    #     return self

    # def push(self, value):
    #     """
    #     Alias for append.

    #     :param value: The value to push
    #     :type value: mixed

    #     :rtype: Collection
    #     """
    #     return self.append(value)

    # def pull(self, key, default=None):
    #     """
    #     Pulls an item from the collection.

    #     :param key: The key
    #     :type key: mixed

    #     :param default: The default value
    #     :type default: mixed

    #     :rtype: mixed
    #     """
    #     val = self.get(key, default)

    #     self.forget(key)

    #     return val

    # def put(self, key, value):
    #     """
    #     Put an item in the collection by key.

    #     :param key: The key
    #     :type key: mixed

    #     :param value: The value
    #     :type value: mixed

    #     :rtype: Collection
    #     """
    #     self[key] = value

    #     return self

    # def reduce(self, callback, initial=None):
    #     """
    #     Reduce the collection to a single value.

    #     :param callback: The callback
    #     :type callback: callable

    #     :param initial: The initial value
    #     :type initial: mixed

    #     :rtype: mixed
    #     """
    #     return reduce(callback, self._cache, initial)

    # def reject(self, callback):
    #     """
    #     Create a collection of all elements that do not pass a given truth test.

    #     :param callback: The truth test
    #     :type callback: callable

    #     :rtype: Collection
    #     """
    #     if self._use_as_callable(callback):
    #         return self.filter(lambda item: not callback(item))

    #     return self.filter(lambda item: item != callback)

    # def reverse(self):
    #     """
    #     Reverse items order.

    #     :rtype: Collection
    #     """
    #     if isinstance(self._cache, dict):
    #         return self

    #     return self.__class__(list(reversed(self._cache)))

    # def shift(self):
    #     """
    #     Remove first item of the collection, and return it.

    #     :rtype: mixed
    #     """
    #     return self.pop(0)

    # def sort(self, callback=None):
    #     """
    #     Sort through each item with a callback.

    #     :param callback: The callback
    #     :type callback: callable or None

    #     :rtype: Collection
    #     """
    #     items = self._cache

    #     if callback:
    #         return self.__class__(sorted(items, key=callback))
    #     else:
    #         return self.__class__(sorted(items))

    # def sum(self, callback=None):
    #     """
    #     Get the sum of the given values.

    #     :param callback: The callback
    #     :type callback: callable or string or None

    #     :rtype: mixed
    #     """
    #     if callback is None:
    #         return sum(self._cache)

    #     callback = self._value_retriever(callback)

    #     return self.reduce(lambda result, item: (result or 0) + callback(item))

    # def take(self, limit):
    #     """
    #     Take the first or last n items.

    #     :param limit: The number of items to take
    #     :type limit: int

    #     :rtype: Collection
    #     """
    #     if limit < 0:
    #         return self[limit:]

    #     return self[:limit]

    # def transform(self, callback):
    #     """
    #     Transform each item in the collection using a callback.

    #     :param callback: The callback
    #     :type callback: callable

    #     :rtype: Collection
    #     """
    #     self._items = self.map(callback).all()

    #     return self

    # def unique(self, key=None):
    #     """
    #     Return only unique items from the collection list.

    #     :param key: The key to chech uniqueness on
    #     :type key: mixed

    #     :rtype: Collection
    #     """
    #     if key is None:
    #         seen = set()
    #         seen_add = seen.add

    #         return self.__class__([x for x in self._cache if not (x in seen or seen_add(x))])

    #     key = self._value_retriever(key)

    #     exists = []

    #     def _check(item):
    #         id_ = key(item)
    #         if id_ in exists:
    #             return True

    #         exists.append(id_)

    #     return self.reject(_check)

    # def values(self):
    #     """
    #     Return collection values.

    #     :rtype: Collection
    #     """
    #     if not isinstance(self._cache, dict):
    #         return self

    #     return self.__class__(list(self._cache.values()))

    # def keys(self):
    #     """
    #     Return collection keys.

    #     :rtype: Collection
    #     """
    #     if not isinstance(self._cache, dict):
    #         return self

    #     return self.__class__(list(self._cache.keys()))

    # def zip(self, *items):
    #     """
    #     Zip the collection together with one or more arrays.

    #     :param items: The items to zip
    #     :type items: list

    #     :rtype: Collection
    #     """
    #     return self.__class__(list(zip(self._cache, *items)))

    # def is_empty(self):
    #     return len(self) == 0

    # def _get_items(self, items):
    #     if isinstance(items, Collection):
    #         items = items.all()
    #     elif hasattr('items', 'to_list'):
    #         items = items.to_list()
    #     elif hasattr('items', 'to_dict'):
    #         items = items.to_dict()

    #     return items

    # def _value_retriever(self, value):
    #     """
    #     Get a value retrieving callback.

    #     :type value: mixed

    #     :rtype: callable
    #     """
    #     if self._use_as_callable(value):
    #         return value

    #     return lambda item: data_get(item, value)

    # def _use_as_callable(self, value):
    #     """
    #     Determine if the given value is callable.

    #     :type value: mixed

    #     :rtype: bool
    #     """
    #     return not isinstance(value, basestring) and callable(value)

    # @deprecated
    # def to_dict(self):
    #     return self.serialize()

    # def serialize(self):
    #     """
    #     Get the collection of items as a serialized object (ready to be json encoded).

    #     :rtype: dict or list
    #     """
    #     def _serialize(value):
    #         if hasattr(value, 'serialize'):
    #             return value.serialize()
    #         elif hasattr(value, 'to_dict'):
    #             return value.to_dict()
    #         else:
    #             return value

    #     if not isinstance(self._cache, dict):
    #         return list(map(_serialize, self._cache))

    #     items = {}
    #     for key, value in self._cache.items():
    #         items[key] = _serialize(value)

    #     return items

    # def to_json(self, **options):
    #     """
    #     Get the collection of items as JSON.

    #     :param options: JSON encoding options:
    #     :type options: dict

    #     :rtype: str
    #     """
    #     return json.dumps(self.serialize(), **options)    

    def is_empty(self):
        return len(self.all()) == 0
 
    def __len__(self):
        return self.count()

    # def __iter__(self):
    #     if self._cache is None:
    #         for item in self._cache
    #     for item in self._cache:
    #         yield item

    def __getitem__(self, item):
        if self._cache is None:
            print '*'*10, self.relation, self.owner_instance
            print '*'*10, self.relation.prefetch(self.owner_instance)
            return self.relation.prefetch(self.owner_instance).limit(1).offset(item).first()
        return self._cache[item]

#     def __setitem__(self, key, value):
#         self._cache[key] = value
# 
#     def __delitem__(self, key):
#         del self._cache[key]
