# -*- coding:utf-8 -*-
from .relation import Relation
from ..utils import *


class HasOne(Relation):
    
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
        self._owner_attr = singularize_of(target.table_name)
        return self._owner_attr

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


def has_one(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasOne(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation
