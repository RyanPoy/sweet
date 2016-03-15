# -*- coding:utf-8 -*-
from sweet.relation.base import Relation
from sweet.criteria import JoinClause
from sweet.utils import *


class HasAndBelongsToMany(Relation):
    
    def __init__(self, target_class, owner_class=None, owner_attr=None, 
                 foreign_key=None, target_foreign_key=None, association_table=None):
        super(HasAndBelongsToMany, self).__init__(target_class, owner_class, owner_attr, foreign_key)
        self._association_table = association_table
        self._target_foreign_key = target_foreign_key

    @property
    def owner_attr(self):
        """ User has many Phone, owner attr must be 'phones'
        """
        if self._owner_attr:
            return self._owner_attr
        if self.owner is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
#         target = self.target
#         if target is None:
#             return None
#         self._owner_attr = pluralize(pythonize(target.__name__))
        if isinstance(self._target, str):
            name = self._target.split('.')[-1]
        elif issubclass(self._target, ActiveRecord):
            name = self._target.__name__
        else:
            return None
        self._owner_attr = pluralize(pythonize(name))
        return self._owner_attr


    @property
    def target_foreign_key(self):
        if self._target_foreign_key:
            return self._target_foreign_key
        target = self.target
        if target is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(target, ActiveRecord):
            return None
        target_foreign_key = singularize(pythonize(target.__name__)) + '_id'
#         if not self.target.has_column(foreign_key):
#             raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._target_foreign_key = target_foreign_key
        return self._target_foreign_key

    @property
    def foreign_key(self):
        """ User has one Phone, foreign key must be user_id, and appear in Phone
        User has many Phone, foreign key must be user_id, and appear in Phone
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
#         if not self.target.has_column(foreign_key):
#             raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key
    
    @property
    def association_table(self):
        if self._association_table:
            return self._association_table
        owner, target = self.owner, self.target
        if owner is None or target is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord) or not issubclass(self.target, ActiveRecord):
            return None
        l = [ tableize(self.owner.__name__), tableize(self.target.__name__) ]
        l.sort()
        self._association_table = '_'.join(l) 
        return self._association_table

    def __get__(self, instance, owner):
        return self._get(instance)

    def _get(self, instance):
        foreign_key_value = getattr(instance, instance.__pk__)

        return self.target.join(
            JoinClause(self.association_table)\
                .on('`%s`.`%s` = ?' % (self.association_table, self.foreign_key), foreign_key_value)\
                .on('`%s`.`%s` = `%s`.`%s`' % (self.association_table, self.target_foreign_key, self.target.table_name, self.target.__pk__))
        ).all()


def has_and_belongs_to_many(target_class_or_classpath, foreign_key=None, target_foreign_key=None, 
                                owner_attr=None, association_table=None):
    relation = HasAndBelongsToMany(target_class=target_class_or_classpath, foreign_key=foreign_key, 
                    target_foreign_key=target_foreign_key, owner_attr=owner_attr, association_table=association_table)
    Relation.push(relation)
    return relation
