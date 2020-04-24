#coding: utf8
from sweet.utils.inflection import *


class ModelHasBeenPersisted(Exception): pass
class ModelHasNotBeenPersisted(Exception): pass


class ModelMetaClass(type):

    def __init__(cls, name, bases, attr):
        model = type.__init__(cls, name, bases, attr)
        if name != 'Model':
            # set __tablename__ to Record Class
            if not hasattr(cls, '__tablename__'):
                setattr(cls, '__tablename__', tableize(cls.__name__))

            if not hasattr(cls, '__field_dict__'):
                setattr(cls, '__field_dict__', {})
                cls._init_fields()

            if not hasattr(cls, '__pk__'):
                setattr(cls, '__pk__', 'id')

            # if cls.__pk__ not in cls.__field_dict__:
            #     raise Exception('%s field of %s does not exist' % (cls.__pk__, cls.__name__))

            if getattr(cls, '__timestamp__', True):
                setattr(cls, 'created_at', None)
                setattr(cls, 'updated_at', None)

            # from sweet.relation import Relation
            # for relation in Relation.iter():
            #     relation.inject(cls)

            # # id column must in columns validate
            # if cls.__pk__ not in cls.__field_dict__:
            #     raise PKColumnNotInColumns()
            # for c, err in [
            #     (cls.__pk__, PKColumnNotInColumns),
            #     (cls.__created_at__, CreatedAtColumnNotInColumns), 
            #     (cls.__updated_at__, UpdatedAtColumnNotInColumns),
            #     (cls.__created_on__, CreatedOnColumnNotInColumns),
            #     (cls.__updated_on__, UpdatedOnColumnNotInColumns),
            # ]:
            #     if c and  c not in cls.__field_dict__:
            #         raise err()
        return model


class Model(metaclass=ModelMetaClass):

    def __init__(self, attrs=None, **kwargs):
        for k, v in attrs.items():
            setattr(self, k, v)
        for k, v in kwargs.items():
            setattr(self, k, v)

    def column_dict(self):
        pass

    def save(self):
        """ save object. 
        will update the record if it has been persisted else create one.
        """
        if self.id:
            self.update()
        else:
            self.create()
        return self

    def create(self):
        """ create a record in db.
        should raise ModelHasBeenPersisted if it has been persisted
        """
        if self.persisted():
            raise ModelHasBeenPersisted()
        self.id = self._rs.insert_getid(self.column_dict())
        return self

    def update(self):
        """ update a record in db, 
        should raise ModelHasNotBeenPersisted if it has not been persisted
        """
        if not self.persisted():
            self._rs.update(self.column_dict())
            return self
        raise ModelHasNotBeenPersisted()

    def delete(self):
        """ delete a record in db and set id is None.
        would do nothing if it has not been persisted
        """
        if self.persisted():
            self._rs.delete()
            self.id = None
        return self

    def persisted(self):
        return True if getattr(self, 'id', None) else False

    @property
    def _rs(self):
        db = self.db_manager.new_db()
        return db.recordset(self.__tablename__)

    @classmethod
    def _init_fields(cls):
        for c in cls.db_manager.new_db().get_columns(cls.__tablename__):
            cls.__field_dict__[c.name] = c
        return cls


