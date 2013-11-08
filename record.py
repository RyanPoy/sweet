#coding: utf8
# # from pyrails.associations import Association
from pyrails.record_manager import RecordManager
from pyrails.decorates import classproperty
from pyrails.inflection import Inflection
from pyrails.db import get_database
# from pyrails.exceptions import RecordValidateError, RecordHasNotBeenPersisted

# from pyrails.support import classproperty, hungarian_name_of
# import re


# class FindMethodMissing(object):

#     __find_by_pattern = re.compile('^find_(all_by|by)_([_a-zA-Z]\w*)$')
#     __find_or_by_pattern = re.compile('^find_or_(init|create)_by_([_a-zA-Z]\w*)$')

#     def __init__(self, instance, method_name):
#         self.__name = method_name
#         self.__model_instance = instance

#     @classmethod
#     def match(cls, name):
#         return cls.__find_by_pattern.match(name) or cls.__find_or_by_pattern.match(name)

#     def __call__(self, *args):
#         def extract_scope_or_method_and_arguments(match, args):
#             groups = match.groups()
#             scope = groups[0]
#             args_name = groups[1].split('_and_')
#             return scope, dict(zip(args_name, args))

#         r = None
#         match = self.__class__.__find_by_pattern.match(self.__name)
#         if match:
#             scope, argments = extract_scope_or_method_and_arguments(match, args)
#             if scope == 'all_by':
#                 r = self.__model_instance.where(argments).all
#             else: # must 'by'
#                 r = self.__model_instance.where(argments).first
#         else:
#             match = self.__class__.__find_or_by_pattern.match(self.__name)
#             if match:
#                 scope, argments = extract_scope_or_method_and_arguments(match, args)
#                 if scope == 'create':
#                     r = self.__model_instance.create(**argments)
#                 else: # must 'init'
#                     r = self.__model_instance(**argments)
#         return r


# class CreateOrBuildMethodMissing(object):

#     __pattern = re.compile('^(create|build)_([_a-zA-Z]\w*)$')

#     def __init__(self, instance, method_name):
#         self.__name = method_name
#         self.__model_instance = instance

#     @classmethod
#     def match(cls, name):
#         return cls.__pattern.match(name)
            
#     def __call__(self, *args, **kwargs):
#         r = None
#         match = self.__class__.__pattern.match(self.__name)
#         if match:
#             scope, association_propert_name = match.groups()
#             association = self.__model_instance.association_dict.get(association_propert_name, None)
#             if not association:
#                 raise AttributeError("'%s' object has no attribute '%s'" % (self.__model_instance.__class__.__name__, self.__name))
#             foreign_key = association.foreign_key
#             if foreign_key not in kwargs:
#                 kwargs[foreign_key] = self.__model_instance.id
#             if scope == 'create':
#                 r = association.target.create(*args, **kwargs)
#             else: # must 'build'
#                 r = association.target(*args, **kwargs)
#             setattr(self.__model_instance, association_propert_name, r)
#         return r
    

class ActiveRecord(object):
    """
    A ORM. You can use it like this:
    - Query with chain 
        - where:
          User.where(name='pengyi').where('age != ?', 28).where('birthday between ? and ?', '1982-01-01', '1986-01-01')
         # => SELECT FROM users WHERE name = 'pengyi' AND age != 28 AND birthday between '1982-01-01' and '1986-01-01'
        
        - limit 
          User.where(name='pengyi').limit(10, 1) 
         # => SELECT FROM users WHERE name = 'pengyi' LIMIT 10 OFFSET 1
         
        - group/order/having ....
          User.where(name='pengyi').order('name DESC').group('name').having(age = 10)
         # => SELECT users.* FROM users WHERE users.name = 'pengyi' GROUP BY users.name HAVING users.age = 10 ORDER BY name DESC LIMIT 10 OFFSET 1
    
    - Query with method missing
      User.find_by_name('pengyi')
     # => SELECT FROM users WHERE name = 'pengyi'

      User.find_by_name_and_age('pengyi', 28)
     # => SELECT FROM users WHERE name = 'pengyi' AND age = 28
     
    - Create a record in database
      User.create(name = 'pengyi', age = 28)
     # => INSERT INTO users (name, age) VALUES ('pengyi', 28)
      
      User(name = 'pengyi', age = 28).save()
     # => INSERT INTO users (name, age) VALUES ('pengyi', 28)
     
    - Update
      u = User.find(1)  # 1 meams id
      u.update_attributes(name = 'poy', age = 30)
     # => UPDATE users SET name = 'poy', age = 30 WHERE id = 1
    
      User.where(age=20).update_all(name='poy', age='40)
     # => UPDATE users SET name = 'poy', age = 40 WHERE age = 20
    
    - Delete
      u = User.find(1)
      u.delete()
     # => DELETE FROM users WHERE id = 1
     
      u = User.where(age=20).delete_all()
     # => DELETE FROM users WHERE age = 20
    """
    __db__ = None

    # class __metaclass__(type):
    #     def __init__(cls, name, bases, attr):
    #         model_instance = type.__init__(cls, name, bases, attr)
    #         # if name != 'ActiveRecord':
    #         #     for assocation in Association._next:
    #         #         assocation._register(cls)
    #         return model_instance 
        
    #     def __getattribute__(self, name):
    #         try:
    #             return type.__getattribute__(self, name)
    #         except AttributeError, _:
    #             if FindMethodMissing.match(name):
    #                 return FindMethodMissing(self, name)
    #             raise

    def __init__(self, **attributes):
        self.id = None
        for k, v in attributes.iteritems():
            setattr(self, k, v)

    # def __getattribute__(self, name):
    #     try:
    #         return super(ActiveRecord, self).__getattribute__(name)
    #     except AttributeError, _:
    #         association = self.association_dict.get(name, None)
    #         # if association:
    #         #     if association._type == Association.Type.belongs_to:
    #         #         return association.target.find(getattr(self, association.foreign_key))
    #         #     elif association._type == Association.Type.has_one:
    #         #         return association.target.where({association.foreign_key: self.id}).first
    #         if CreateOrBuildMethodMissing.match(name):
    #             return CreateOrBuildMethodMissing(self, name)
    #         raise

    @classproperty
    def table_name(cls):
        if not hasattr(cls, '__table_name__'):
            table_name = Inflection.pluralize(cls.__name__)
            cls.__table_name__ = Inflection.hungarian_name_of(table_name)
        return cls.__table_name__

    @classproperty
    def table_name_sql(cls):
        if not hasattr(cls, '__table_name_sql__'):
            cls.__table_name_sql__ = '`%s`' % cls.table_name
        return cls.__table_name_sql__

    @classproperty
    def column_names(cls):
        if not hasattr(cls, '__column_names__'):
            cls.__column_names__ = [ x.name for x in cls._get_db().get_table_by(cls.table_name).columns ]
        return cls.__column_names__

    @classproperty
    def column_names_sql(cls):
        if not hasattr(cls, '__column_names_sql__'):
            cls.__column_names_sql__ = ', '.join( map(lambda x: '`%s`' % x, cls.column_names) )
        return cls.__column_names_sql__
            
    @classproperty
    def column_placeholder_sql(cls):
        if not hasattr(cls, '__column_placeholder_sql__'):
            cls.__column_placeholder_sql__ = ', '.join( ['?'] * len(cls.column_names) )
        return cls.__column_placeholder_sql__
    
    # @classproperty
    # def association_dict(cls):
    #     if not hasattr(cls, '__association_dict__'):
    #         cls.__association_dict__ = {}
    #     return cls.__association_dict__
    
    # @classproperty
    # def all(cls):
    #     return RecordManager(cls).all
    
    # @classproperty
    # def first(cls):
    #     return RecordManager(cls).first
    
    # @classproperty
    # def last(cls):
    #     return RecordManager(cls).last

    @classmethod
    def create(cls, **attributes):
        """ persist a active record. 
        @return：record instace if successful, else return None. 
                it will throw exception when any exception occur
        eg. 
            User.create(username='abc', password='123')
        """
        record = cls(**attributes)
        return record if record.save() else None

    # @classmethod
    # def create_(cls, hash_attributes={},  **kw_attributes):
    #     record = cls(hash_attributes, **kw_attributes)
    #     record.save_()
    #     return record
    
    @classmethod
    def where(cls, *args, **kwargs):
        return RecordManager(cls).where(*args, **kwargs)

    @classmethod
    def find(cls, *ids):
        """ find record by ids
        @return a record array if all found. else throw RecordNotFound exception
        """
        return RecordManager(cls).find(*ids)

    # @classmethod
    # def limit(cls, limit=0, offset=0):
    #     return RecordManager(cls).limit(limit, offset)

    # @classmethod
    # def count(cls):
    #     return RecordManager(cls).count()
    
    # @classmethod
    # def sum(cls, attribute_name):
    #     return RecordManager(cls).sum(attribute_name)

    # @classmethod
    # def order(cls, *args):
    #     if args:
    #         return RecordManager(cls).order(args[0]) 
    #     return RecordManager(cls)

    # @classmethod
    # def group(cls, group):
    #     return RecordManager(cls).group(group)

    # @classmethod
    # def having(cls, *args, **kwargs):
    #     return RecordManager(cls).having(*args, **kwargs)

    # @classmethod
    # def join(cls, *joins):
    #     return RecordManager(cls).join(*joins)
    
    def valid(self):
        return True
    
    def save(self):
        """ persist a record instance.
        @return：record instace if successful, else return None. 
                it will throw exception when any exception occur
        eg.
            u = User(username="abc", password="123456").save()
        """
        if not self.valid():
            raise False

        if self.is_persisted():
            attrs_dict = dict([ (column_name, getattr(self, column_name)) for column_name in self.column_names if column_name != 'id'])
            self.where(id = self.id).update_all(attrs_dict)
            return self
        else:
            return RecordManager(self.__class__).save(self)
 
    def update_attributes(self, **attributes):
        """ update attributes
        eg. 
            u = User(username="abc", password="123")
            u.update_attributes(username="efg", password="456")
        """
        # def filter_of(attributes_dict):
        #     other = {}
        #     for attr_name, attr_value in attributes_dict.iteritems():
        #         if attr_name and hasattr(self, attr_name) and getattr(self, attr_name) != attr_value:
        #             other[attr_name] = attr_value
        #     return other

        # if not self.is_persisted():
        #     raise RecordHasNotBeenPersisted()

        if not self.valid():
            raise False

        # all_args_attributes = {}
        # all_args_attributes.update(attributes)
        # attributes = filter_of(all_args_attributes)
        self.where(id = self.id).update_all(**attributes)
        for name, value in attributes.iteritems():
            setattr(self, name, value)
        return True
        
    @classmethod
    def update_all(cls, **attributes):
        RecordManager(cls).update_all(**attributes)
        return True

    def delete(self):
        """delete a record instance.
        eg.
            u = User.find(1)
            u.delete()
        """
        return self.where(id=self.id).delete_all()

    @classmethod
    def delete_all(cls):
        """ delete all records
        eg.
            User.delete_all()
            User.find(1, 2, 3).delete_all()
        """
        RecordManager(cls).delete_all()
        return True

    @classmethod
    def _get_db(cls):
        if cls.__db__ is None:
            cls.__db__ = get_database()
        return cls.__db__
    
    def is_persisted(self):
        return self.id is not None
