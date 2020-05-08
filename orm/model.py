#coding: utf8
from sweet.utils import *
from sweet.utils.inflection import *
from sweet.orm.relations import *


class ModelHasBeenPersisted(Exception): pass
class ModelHasNotBeenPersisted(Exception): pass


class ModelMetaClass(type):

    def __init__(cls, name, bases, attr):
        # print ("^"*5, " meta class init ", "^"*5, cls, name, bases, attr)

        if name != 'Model':
            # set __tablename__ to model Class
            if not hasattr(cls, '__tablename__'):
                setattr(cls, '__tablename__', tableize(cls.__name__))

            if not hasattr(cls, '__field_define_dict__'):
                setattr(cls, '__field_define_dict__', {})
                cls._init_fields()

            if not hasattr(cls, '__pk__'):
                setattr(cls, '__pk__', 'id')

            if not hasattr(cls, '__relations__'):
                setattr(cls, '__relations__', {})

            # if cls.__pk__ not in cls.__field_define_dict__:
            #     raise Exception('%s field of %s does not exist' % (cls.__pk__, cls.__name__))

            if getattr(cls, '__timestamp__', True):
                setattr(cls, 'created_at', None)
                setattr(cls, 'updated_at', None)

            while True:
                r = relation_q.get()
                if r:
                    r.inject(cls)
                else:
                    break

            # # id column must in columns validate
            # if cls.__pk__ not in cls.__field_define_dict__:
            #     raise PKColumnNotInColumns()
            # for c, err in [
            #     (cls.__pk__, PKColumnNotInColumns),
            #     (cls.__created_at__, CreatedAtColumnNotInColumns), 
            #     (cls.__updated_at__, UpdatedAtColumnNotInColumns),
            #     (cls.__created_on__, CreatedOnColumnNotInColumns),
            #     (cls.__updated_on__, UpdatedOnColumnNotInColumns),
            # ]:
            #     if c and  c not in cls.__field_define_dict__:
            #         raise err()

        return type.__init__(cls, name, bases, attr)


class Model(metaclass=ModelMetaClass):

    def __init__(self, **attrs):
        for k, v in attrs.items():
            setattr(self, k, v)
        self._init_field_default_value() # !Very Important

    def __getattribute__(self, name):
        try:
            return super().__getattribute__(name)
        except AttributeError as ex:
            if name in self.__relations__:
                r = self.__relations__[name]
                return r.get_real_value(self)
            raise ex

    def __setattr__(self, name, value):
        cls = self.__class__
        relt = super().__setattr__(name, value)
        if name in cls.__relations__:
            relation = cls.__relations__[name]
            fk = relation.fk
            fk_value = value.get_pk()
            setattr(self, fk, fk_value)
        return relt

    def _init_field_default_value(self):
        """ set default value of field which does not init 
        """
        cls = self.__class__
        for name, field in cls.__field_define_dict__.items():
            if not hasattr(self, name):
                setattr(self, name, field.default)
        return self

    def column_dict(self):
        d = {
            name:getattr(self, name, field.default) \
                for name, field in self.__field_define_dict__.items() 
        }
        return d

    def save(self):
        """ save object. 
        will update the record if it has been persisted else create one.
        """
        if self.persisted():
            self.update()
        else:
            _id = self.objects.insert_getid(**self.column_dict())
            self.set_pk(_id)
        return self

    @classmethod
    def create(cls, **attrs):
        """ create a record in db.
        should raise ModelHasBeenPersisted if it has been persisted
        """
        model = cls(**attrs)
        model.save()
        return model

    @classmethod
    def create_all(cls, *items):
        """ batch create and return how many insert successful
        """
        return cls.objects.insert(items)

    def update(self, **attrs):
        """ update a record in db, 
        should raise ModelHasNotBeenPersisted if it has not been persisted
        """
        if not self.persisted():
            raise ModelHasNotBeenPersisted()
        if attrs:
            old_values = { 
                k: getattr(self, k) for k, v in attrs.items() if hasattr(self, k)
            }
            for k, v in attrs.items():
                setattr(self, k, v)
            self.objects.update(**self.column_dict())
        else:
            self.objects.update(**self.column_dict())
        return self

    @classmethod
    def update_all(cls, **attrs):
        """ delete all record
        """
        cls.objects.update(**attrs)
        return cls

    def delete(self):
        """ delete a record in db and set id is None.
        would do nothing if it has not been persisted
        """
        self.__class__._delete_relations([self])

        if self.persisted():
            pk = self.__pk__
            self.objects.where(**{self.__pk__: self.get_pk()}).delete()
            self.set_pk(None)
        return self

    @classmethod
    def delete_all(cls, **attrs):
        """ delete all record
        """
        if attrs:
            objs = cls.objects.where(**attrs)
        else:
            objs = cls.objects

        rs = objs.all() if cls.__relations__ else []
        cls._delete_relations(rs)

        objs.delete()
        return cls

    @classmethod
    def _delete_relations(cls, owner_models):
        for name, r in cls.__relations__.items():
            r.delete_all_real_value(owner_models)


    def set_pk(self, value):
        return setattr(self, self.__pk__, value)

    def get_pk(self):
        return getattr(self, self.__pk__, None)

    def persisted(self):
        return True if self.get_pk() else False

    @classmethod
    def count(cls, column=None, distinct=False):
        return cls.objects.count(column, distinct)

    @classmethod
    def truncate(cls):
        return cls.objects.truncate()

    @classmethod
    def exists(cls):
        return cls.objects.exists()

    @classmethod
    def max(cls, column, distinct=False):
        return cls.objects.max(column, distinct)

    @classmethod
    def min(cls, column, distinct=False):
        return cls.objects.min(column, distinct)

    @classmethod
    def avg(cls, column, distinct=False):
        return cls.objects.avg(column, distinct)

    @classmethod
    def sum(cls, column, distinct=False):
        return cls.objects.sum(column, distinct)

    @classmethod
    def all(cls):
        return cls.objects.all()

    @classmethod
    def first(cls):
        return cls.objects.first()

    @classmethod
    def last(self):
        return self.objects.last()

    @classmethod
    def find(cls, *ids):
        """ find by ids. there are 2 cases.
        case 1: one id，should return model instance if found else None
        case 2: multiple ids, should return the model list
        """
        if len(ids) == 1:
            return cls.objects.where(id=ids).first()
        else:
            return cls.objects.where(id=ids).all()

    @classmethod
    def find_or_create(cls, **kwargs):
        model = cls.objects.where(**kwargs).limit(1).first()
        if not model:
            model = cls.create(**kwargs)
        return model

    @classproperty
    def objects(cls):
        db = cls.db_manager.new_db()
        rs = db.records(cls.__tablename__)
        rs.model_class = cls
        return rs

    @classmethod
    def _init_fields(cls):
        for c in cls.db_manager.new_db().get_columns(cls.__tablename__):
            cls.__field_define_dict__[c.name] = c
        return cls

    @classmethod
    def _register_relation(cls, name, r):
        cls.__relations__[name] = r

    @classmethod
    def select(cls, *columns):
        return cls.objects.select(*columns)

    @classmethod
    def where(cls, **filters):
        return cls.objects.where(**filters)
