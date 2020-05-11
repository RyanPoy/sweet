#coding: utf8
from sweet.orm.relations.relation import relation_q
from sweet.orm.relations.has_many import HasMany
from sweet.utils.inflection import *


class HasOne(HasMany):

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return self.target.where(**{self.fk: owner_obj.get_pk()}).first()


def has_one(name, clazz, fk=None, pk=None, cascade=False):
    r = HasOne(target=clazz, name=name, fk=fk, pk=pk, cascade=cascade)
    relation_q.put(r)
