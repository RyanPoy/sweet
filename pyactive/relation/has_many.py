# -*- coding:utf-8 -*-
from .relation import Relation
from ..utils import *


class HasMany(object):
    
    def __init__(self, target_class, owner_class=None, owner_attr=None, foreign_key=None):
        """
        target_class: 目标的 class
        owner_class: 拥有者的class
        foreign_key: 目标的 class 所在table的字段名
        """
        self._owner = owner_class
        self._target = target_class
        self._foreign_key = foreign_key
        self._owner_attr = owner_attr
        
    @property
    def owner(self):
        return self._owner

    @property
    def target(self):
        """_target is None, should be return None
           _target is a active record class path, should be parse it and set a active record class to _target, then return it
           _target is a active record class, should be return it   
        """
        if self._target is None:
            return None
        if isinstance(self._target, str):
            self._target = import_object(self._target)
        return self._target
    
    @property
    def owner_attr(self):
        if self._owner_attr:
            return self._owner_attr
        if self.owner is None:
            return None
        from ..record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        target = self.target
        if target is None:
            return None
        self._owner_attr = pluralize_of(target.table_name)
        return self._owner_attr

    @property
    def target_pk_column(self):
        target = self.target
        return target.__pk__ if target else None

    @property
    def foreign_key(self):
        """Phone belongs to User
        owner_column must be user_id
        """
        if self._foreign_key:
            return self._foreign_key
        if self.owner is None:
            return None
        from ..record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        owner = self.owner
        if owner is None:
            return None
        foreign_key = singularize_of(owner.table_name) + '_id'
        if not self.target.has_column(foreign_key):
            raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key

    def __get__(self, instance, owner):
        foreign_key_value = getattr(instance, instance.__pk__)
        return self.target.where(**{self.foreign_key: foreign_key_value}).first()

    def inject(self, owner=None):
        if owner: self._owner = owner
        self._owner.__relations__.append(self)
        setattr(self._owner, self.owner_attr, self)


def has_many(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasMany(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation
