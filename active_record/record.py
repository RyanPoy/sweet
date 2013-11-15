#coding: utf8
from pyrails.active_record.method_missing import FindMethodMissing, CreateOrBuildMethodMissing
from pyrails.active_record.sql_builder import SQLBuilder
from pyrails.active_record.associations import Association
from pyrails.active_support import classproperty, Inflection
from pyrails.db import get_database
# from pyrails.active_support import RecordValidateError, RecordHasNotBeenPersisted


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

    class __metaclass__(type):
        def __init__(cls, name, bases, attr):
            model_instance = type.__init__(cls, name, bases, attr)
            if name != 'ActiveRecord':
                for assocation in Association._next:
                    assocation._register(cls)
            return model_instance 
        
        def __getattribute__(self, name):
            try:
                return type.__getattribute__(self, name)
            except AttributeError, _:
                if FindMethodMissing.match(name):
                    return FindMethodMissing(self, name)
                raise

    def __init__(self, **attributes):
        self.id = None
        for k, v in attributes.iteritems():
            setattr(self, k, v)

    def __getattribute__(self, name):
        try:
            return super(ActiveRecord, self).__getattribute__(name)
        except AttributeError, _:
            association = self.association_of(name)
            if association:
                if association._type == Association.Type.belongs_to:
                    # A belongs_to B
                    # A.B  => "SEELCT B.* FROM B WHERE id = A.B_id"
                    return association.target.find(getattr(self, association.foreign_key))
                elif association._type == Association.Type.has_one:
                    # A has_one B
                    # A.B => "SELECT B.* FROM B WHERE A_id = A.id"
                    return association.target.where(**{association.foreign_key: self.id}).first
            if CreateOrBuildMethodMissing.match(name):
                return CreateOrBuildMethodMissing(self, name)
            raise

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
    
    @classproperty
    def association_dict(cls):
        if not hasattr(cls, '__association_dict__'):
            cls.__association_dict__ = {}
        return cls.__association_dict__

    @classmethod
    def association_of(cls, name):
        return cls.association_dict.get(name, None)
    
    @classproperty
    def all(cls):
        """ find all records
        @return a record array
        eg.
            User.all
            User.where(age=10).all
        """
        return SQLBuilder(cls).all
    
    @classproperty
    def first(cls):
        """ find the first record
        @return a record
        eg.
            User.first
            User.where(age=10).first
        """
        return SQLBuilder(cls).first
    
    @classproperty
    def last(cls):
        """ find the last record
        @return a record
        eg.
            User.last
            User.where(age=10).last
        """
        return SQLBuilder(cls).last

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
    
    @classmethod
    def where(cls, *args, **kwargs):
        """ condition query
        eg.
            User.where(username="abc").where(password="123")
            User.where("username='abc' and password='123'")
            User.where("username=? and password=?", 'abc', '123')
        """
        return SQLBuilder(cls).where(*args, **kwargs)

    @classmethod
    def find(cls, *ids):
        """ find record by ids
        @return:    if there is a id and found it will return a record
                    if there are many ids and found them will return a record array
                    if any id not found, throw RecordNotFound exception
        """
        return SQLBuilder(cls).find(*ids)

    @classmethod
    def limit(cls, limit=0, offset=0):
        """ limit query
        eg. 
            User.limit(10)
            User.limit(10, 1)
        """
        return SQLBuilder(cls).limit(limit, offset)

    @classmethod
    def count(cls):
        """ get count number
        @return a number
        eg. 
            User.count()
            User.where('username=123').count()
        """
        return SQLBuilder(cls).count()
    
    @classmethod
    def sum(cls, attribute_name):
        """ get sum number
        @return number
        eg.
            User.sum(age)
            User.where(username=123).sum()
        """
        return SQLBuilder(cls).sum(attribute_name)

    @classmethod
    def order(cls, *args):
        """ order query
        eg.
            User.order('age')
            User.order('age ASC')
            User.order('age DESC')
        """
        if args:
            return SQLBuilder(cls).order(args[0]) 
        return SQLBuilder(cls)

    @classmethod
    def group(cls, group):
        """ group query
        eg. 
            User.group('username')
        """
        return SQLBuilder(cls).group(group)

    @classmethod
    def having(cls, *args, **kwargs):
        """ having query when group
        Note: if there is not use group, the having will be not useful
        eg.
            User.group('username').having(age=1)
        """
        return SQLBuilder(cls).having(*args, **kwargs)

    @classmethod
    def joins(cls, *joins):
        """
        Joining a Single Association
            Category.joins('posts')
        # => SELECT categories.* FROM categories
               INNER JOIN posts ON posts.category_id = categories.id

        Joining Multiple Associations
            Post.joins('category', 'comments')
        # => SELECT posts.* FROM posts
               INNER JOIN categories ON posts.category_id = categories.id
               INNER JOIN comments ON comments.post_id = posts.id
        
        Joining Nested Associations (Single Level)
            Post.joins({'comments': 'guest'})
        # => SELECT posts.* FROM posts
               INNER JOIN comments ON comments.post_id = posts.id
               INNER JOIN guests ON guests.comment_id = comments.id

        Joining Nested Associations (Multiple Level)
            Category.joins({ 'posts': [{'comments': 'guest'}, 'tags'])
        # => SELECT categories.* FROM categories
               INNER JOIN posts ON posts.category_id = categories.id
               INNER JOIN comments ON comments.post_id = posts.id
               INNER JOIN guests ON guests.comment_id = comments.id
               INNER JOIN tags ON tags.post_id = posts.id
        """
        return SQLBuilder(cls).joins(*joins)
    
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
            return SQLBuilder(self.__class__).save(self)
 
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
        SQLBuilder(cls).update_all(**attributes)
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
        SQLBuilder(cls).delete_all()
        return True

    @classmethod
    def _get_db(cls):
        if cls.__db__ is None:
            cls.__db__ = get_database()
        return cls.__db__
    
    def is_persisted(self):
        return self.id is not None
