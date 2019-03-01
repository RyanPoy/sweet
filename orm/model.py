#coding: utf8
from sweet.utils.inflection import *


class ModelMetaClass(type):
    
    def __init__(cls, name, bases, attr):
        model = type.__init__(cls, name, bases, attr)
        if name != 'Model':
            # set __tablename__ to Record Class
            if not hasattr(cls, '__tablename__'):
                setattr(cls, '__tablename__', tableize(cls.__name__))

            if not hasattr(cls, '__pk__'):
                setattr(cls, '__pk__', 'id')

            if getattr(cls, '__timestamp__', True):
                setattr(cls, 'created_at', None)
                setattr(cls, 'updated_at', None)

            # from sweet.relation import Relation
            # for relation in Relation.iter():
            #     relation.inject(cls)

            # # id column must in columns validate
            # if cls.__pk__ not in cls.__columns__:
            #     raise PKColumnNotInColumns()
            # for c, err in [
            #     (cls.__pk__, PKColumnNotInColumns),
            #     (cls.__created_at__, CreatedAtColumnNotInColumns), 
            #     (cls.__updated_at__, UpdatedAtColumnNotInColumns),
            #     (cls.__created_on__, CreatedOnColumnNotInColumns),
            #     (cls.__updated_on__, UpdatedOnColumnNotInColumns),
            # ]:
            #     if c and  c not in cls.__columns__:
            #         raise err()
        return model


class Model(metaclass=ModelMetaClass):
        
    def column_dict(self):
        pass

    def save(self):
        if self.id:
            self.update()
        else:
            self.id = self.db.recordset(self.__tablename__).insert_getid(self.column_dict())
        return self

    def update(self):
        if self.id:
            self.db.recordset(self.__tablename__).update(self.column_dict())
        else:
            self.save()
        return self
