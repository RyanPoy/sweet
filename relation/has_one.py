#coding: utf8
from sweet.relation.base import Relation
from sweet.utils import *


class HasOne(Relation):
    
    @property
    def foreign_key(self):
        """ User has one Phone, foreign key must be user_id, and appear in Phone
        """
        if self._foreign_key:
            return self._foreign_key
        owner = self.owner
        if owner is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        foreign_key = singularize(pythonize(owner.__name__)) + '_id'
        if not self.target.has_column(foreign_key):
            raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key

    @property
    def owner_attr(self):
        """ User has one Phone, owner attr must be 'phone'
        """
        if self._owner_attr:
            return self._owner_attr
        if self.owner is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
#     	target = self.target
#     	if target is None:
#     	    return None
        if isinstance(self._target, str):
            name = self._target.split('.')[-1]
        elif issubclass(self._target, ActiveRecord):
            name = self._target.__name__
        else:
            return None
        self._owner_attr = singularize(pythonize(name))
        return self._owner_attr

    def __get__(self, instance, owner):
        return self.target.where(**{self.foreign_key: instance.pk}).first()

    def _delete(self, owner_instance):
        return self.target.where(**{self.foreign_key: owner_instance.pk}).delete()


def has_one(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasOne(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation
