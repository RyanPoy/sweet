#coding: utf8
from sweet.orm.relations.relation import relation_q
from sweet.orm.relations.has_many import HasMany
from sweet.orm.relations.has_many_through import HasManyThrough
from sweet.utils.inflection import *


class HasOne(HasMany):

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return super().get_real_value(owner_obj).first()


class HasOneThrough(HasManyThrough):

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return super().get_real_value(owner_obj).first()


def has_one(name, clazz, fk=None, cascade=False,
                    through=None, through_fk_on_owner=None, through_fk_on_target=None):
    if not through:
        r = HasOne(target=clazz, name=name, fk=fk, cascade=cascade)
    else:
        r = HasOneThrough(target=clazz, name=name, through=through, through_fk_on_owner=through_fk_on_owner, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
