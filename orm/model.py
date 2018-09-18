#coding: utf8
from sweet.utils.inflection import *

class ModelMetaClass(type):
    
    def __init__(cls, name, bases, attr):
        model = type.__init__(cls, name, bases, attr)
        if name != 'Model':
            # set __tbname__ to Record Class
            if not hasattr(cls, '__tbname__'):
                setattr(cls, '__tbname__', tableize(cls.__name__))

            if not hasattr(cls, '__pk__'):
                setattr(cls, '__pk__', 'id')

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
    pass
