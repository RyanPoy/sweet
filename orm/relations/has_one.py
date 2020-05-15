#coding: utf8
from sweet.orm.relations.relation import relation_q
from sweet.orm.relations.has_many import HasMany
from sweet.orm.relations.has_many_through import HasManyThrough
from sweet.utils.inflection import *


class HasOne(HasMany):

    @property
    def name(self):
        if not self._name:
            self._name = pythonize(singularize(self.target_name))
        return self._name

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return super().get_real_value(owner_obj).first()

    def preload(self, owner_objs):
        target_fks = list(set([ o.get_pk() for o in owner_objs ]))
        if target_fks:
            target_groups = {}
            for t in self.target.where(**{ self.target_fk : target_fks}).all():
                fk = getattr(t, self.target_fk)
                if fk not in target_groups:
                    target_groups[fk] = t

            for o in owner_objs:
                target = target_groups.get(o.get_pk(), None)
                o._set_relation_cache(self.name, target)
        return self


class HasOneThrough(HasManyThrough):

    @property
    def name(self):
        if not self._name:
            self._name = pythonize(singularize(self.target_name))
        return self._name

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return super().get_real_value(owner_obj).first()


def has_one(clazz, name=None, fk=None, cascade=False,
                    through=None, through_fk_on_owner=None, through_fk_on_target=None):
    if not through:
        r = HasOne(target=clazz, name=name, fk=fk, cascade=cascade)
    else:
        r = HasOneThrough(target=clazz, name=name, through=through, through_fk_on_owner=through_fk_on_owner, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
