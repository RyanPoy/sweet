# -*- coding:utf-8 -*-
from collections import namedtuple
from pyrails.exceptions import UnsupportAssociation
from pyrails.support import is_str, import_object
from pyrails.decorates import classproperty
from pyrails.inflection import Inflection


class Association(object):
    
    Type = namedtuple('Type', ['belongs_to', 'has_one', 'has_many'])._make(['belongs_to', 'has_one', 'has_many'])
    
    __associations = []
    
    def __init__(self, target_class_or_classpath, _type, attr_name = None, foreign_key = None, dependent = False):
        """
        attr_name 最终结果：自定义 或者 target_class_or_classpath的最后部分的小写的单数(has_one, belongs_to)或者复数(has_many)
        foreign_key   最终结果：自定义 或者 当belongs_to的时候为：attr_name的单数 + "_id"
                                         当 has_one, has_many的时候，为：类名 + '_id'
        """
        self.__target_class_or_classpath = target_class_or_classpath
        self._type = _type

        _target_name = self.__extract_target_name()
        self._has_input_attr_name = attr_name is not None
        self._has_input_foreign_key = foreign_key is not None
        self.attr_name = attr_name or self.__extract_attr_name(_target_name)
        self.foreign_key   = foreign_key or self.__extract_foreign_key(_target_name)
        self.dependent     = dependent
        
        self.__class__.__associations.append(self)

    def __extract_target_name(self):
        """
        should be return aaa_bbb or ccc string value
        """
        from pyrails.record import ActiveRecord

        if is_str(self.__target_class_or_classpath):
            target_name_str = self.__target_class_or_classpath.split('.')[-1]
        elif issubclass(self.__target_class_or_classpath, ActiveRecord):
            target_name_str = self.__target_class_or_classpath.__name__
        else:
            raise UnsupportAssociation()
        return target_name_str

    def __extract_attr_name(self, _target_name):
        if self._type == self.Type.has_many:
            _target_name = Inflection.hungarian_name_of(Inflection.pluralize(_target_name))
        else: # belongs_to, has_one
            _target_name = Inflection.hungarian_name_of(_target_name)
        return _target_name
    
    def __extract_foreign_key(self, _target_name):
        if self._type == self.Type.has_many:
            _target_name = Inflection.singularize(self.attr_name)
        else: # belongs_to, has_one
            _target_name = self.attr_name
        return '%s_id' % _target_name
    
    @property
    def target(self):
        if is_str(self.__target_class_or_classpath):
            self.__target_class_or_classpath = import_object(self.__target_class_or_classpath)
        return self.__target_class_or_classpath

    @classproperty
    def _next(cls):
        while cls.__associations:
            yield cls.__associations.pop()

    def __str__(self):
        return 'attr_name[%s]; foreign_key[%s]; dependent[%s]; target[%s]' % (self.attr_name, self.foreign_key, self.dependent, self.target)
    
    def _register(self, owner_class):
        if self._type == Association.Type.has_one or self._type == Association.Type.has_many:
            if not self._has_input_foreign_key:
                self.foreign_key = '%s_id' % Inflection.hungarian_name_of(owner_class.__name__)
        owner_class.association_dict[self.attr_name] = self

        
def belongs_to(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False):
    return Association(target_class_or_classpath, Association.Type.belongs_to, attr_name, foreign_key, dependent)
    

def has_one(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False):
    return Association(target_class_or_classpath, Association.Type.has_one, attr_name, foreign_key, dependent)


def has_many(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False):
    return Association(target_class_or_classpath, Association.Type.has_many, attr_name, foreign_key, dependent)


#class has_and_belongs_to_many(Association):
#    def __register__(self, owner):
#        self.target._has_and_belongs_to_many.add(owner)
#        owner._has_and_belongs_to_many.add(self.target)
