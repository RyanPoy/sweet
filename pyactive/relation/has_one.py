# -*- coding:utf-8 -*-
from .relation_methods import owner_attr_for_has_one_and_has_belongs_to, foreign_key_for_has_one_and_has_many
from .relation import Relation


class HasOne(Relation):
    
    owner_attr = property(owner_attr_for_has_one_and_has_belongs_to)
    foreign_key = property(foreign_key_for_has_one_and_has_many)

    def _get(self, instance, owner):
        foreign_key_value = getattr(instance, instance.__pk__)
        return self.target.where(**{self.foreign_key: foreign_key_value}).first()


def has_one(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasOne(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation
