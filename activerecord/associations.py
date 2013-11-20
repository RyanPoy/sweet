#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from collections import namedtuple
from pyrails.activesupport import is_str, import_object, classproperty, Inflection


association_types = ['belongs_to', 'has_one', 'has_many', 'has_and_belongs_to_many']
class Association(object):
    
    Type = namedtuple('Type', association_types)._make(association_types)
    
    __associations = []
    
    def __init__(self, target_class_or_classpath, _type, attr_name=None, foreign_key=None, dependent=False, through=None, join_table=None):
        """
        attr_name 最终结果：自定义 或者 target_class_or_classpath的最后部分的小写的单数(has_one, belongs_to)或者复数(has_many)
        foreign_key   最终结果：自定义 或者 当belongs_to的时候为：attr_name的单数 + "_id"
                                         当 has_one, has_many的时候，为：类名 + '_id'
                    注意：foreign_key 永远是对当前的 class 而言，而不是针对target而言的
        """
        self.__target_class_or_classpath = target_class_or_classpath
        self._type          = _type

        self.attr_name      = attr_name or self.__extract_attr_name()
        self.foreign_key    = foreign_key
        self.dependent      = dependent
        self.through        = through
        self.join_table     = join_table
        
        self.__class__.__associations.append(self)

    def is_belongs_to(self):
        return self._type == self.__class__.Type.belongs_to

    def is_has_one(self):
        return self._type == self.__class__.Type.has_one

    def is_has_many(self):
        return self._type == self.__class__.Type.has_many

    def is_has_and_belongs_to_many(self):
        return self._type == self.__class__.Type.has_and_belongs_to_many
        
    def __extract_target_name(self):
        """
        should be return aaa_bbb or ccc string value
        """
        return self.__target_class_or_classpath.split('.')[-1] if is_str(self.__target_class_or_classpath) \
            else self.__target_class_or_classpath.__name__

    def __extract_attr_name(self):
        _target_name = self.__extract_target_name()
        if self.is_has_many() or self.is_has_and_belongs_to_many():
            _target_name = Inflection.hungarian_of(Inflection.pluralize_of(_target_name))
        else: # belongs_to, has_one
            _target_name = Inflection.hungarian_of(_target_name)
        return _target_name
    
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
        if not self.foreign_key:
            if self.is_has_one() or self.is_has_many():
                self.foreign_key = '%s_id' % Inflection.hungarian_of(owner_class.__name__)
            else: # belongs_to or has_and_belongs_to_many
                self.foreign_key = '%s_id' % Inflection.hungarian_of(self.__extract_target_name())
        if not self.join_table:
            if self.is_has_and_belongs_to_many():
                target_name = Inflection.hungarian_of(Inflection.pluralize_of(self.__extract_target_name()))
                owner_name = Inflection.hungarian_of(Inflection.pluralize_of(owner_class.__name__))
                if owner_name > target_name:
                    self.join_table = '%s_%s' % (target_name, owner_name)
                else:
                    self.join_table = '%s_%s' % (owner_name, target_name)
        owner_class.association_dict[self.attr_name] = self

        
def belongs_to(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False):
    return Association(target_class_or_classpath, Association.Type.belongs_to, attr_name, foreign_key, dependent)
    

def has_one(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False):
    return Association(target_class_or_classpath, Association.Type.has_one, attr_name, foreign_key, dependent)


def has_many(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False, through=None):
    return Association(target_class_or_classpath, Association.Type.has_many, attr_name, foreign_key, dependent, through)

def has_and_belongs_to_many(target_class_or_classpath, attr_name=None, foreign_key=None, dependent=False, join_table=None):
    return Association(target_class_or_classpath, Association.Type.has_and_belongs_to_many, attr_name, foreign_key, dependent, join_table)
