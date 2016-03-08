# -*- coding:utf-8 -*-
from .relation_methods import owner_attr_for_has_one_and_has_belongs_to, foreign_key_for_belongs_to
from .relation import Relation


class BelongsTo(Relation):
    
    owner_attr = property(owner_attr_for_has_one_and_has_belongs_to)
    foreign_key = property(foreign_key_for_belongs_to)
    
    def __get__(self, instance, owner):
        foreign_key_value = getattr(instance, self.foreign_key)
        return self.target.where(**{self.target_pk_column: foreign_key_value}).first()
    
    
def belongs_to(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = BelongsTo(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation

