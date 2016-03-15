#coding: utf8
from sweet.relation.base import Relation
from sweet.record import ActiveRecord
from sweet.utils import *


class BelongsTo(Relation):

    @property
    def foreign_key(self):
        """ Phone belongs to User, foreign key must be user_id, and appear in Phone
        """
        if self._foreign_key:
            return self._foreign_key
        if self.owner is None:
            return None
        if not issubclass(self.owner, ActiveRecord):
            return None
        target = self.target
        if target is None:
            return None
        foreign_key = pythonize(target.__name__) + '_id'
        if not self.owner.has_column(foreign_key):
            raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.owner.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key

    @property
    def owner_attr(self):
        """ Phone belongs to User, owner attr must be 'user'
        """
        if self._owner_attr:
            return self._owner_attr
        if self.owner is None:
            return None
        if not issubclass(self.owner, ActiveRecord):
            return None
#         target = self.target
#         if target is None:
#             return None
        if isinstance(self._target, str):
            name = self._target.split('.')[-1]
        elif issubclass(self._target, ActiveRecord):
            name = self._target.__name__
        else:
            return None
        self._owner_attr = singularize(pythonize(name))
        return self._owner_attr

    def __get__(self, instance, owner):
        foreign_key_value = getattr(instance, self.foreign_key)
        return self.target.where(**{self.target_pk_column: foreign_key_value}).first()
    
    def _delete(self, owner_instance):
        pass

        
def belongs_to(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = BelongsTo(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation
