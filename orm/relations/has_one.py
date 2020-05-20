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

    def preload(self, owner_objs):
        owner_pks = list(set([ o.get_pk() for o in owner_objs ]))
        if not owner_pks:
            return self

        # 1. found the through objects
        through_objs = self.through.objects.where(**{self.through_fk_on_owner: owner_pks}).all()
        if not through_objs:
            return self

        # 2. found the target objects
        target_pks = [ getattr(t, self.through_fk_on_target) for t in through_objs ]
        if not target_pks:
            return self

        target_objs = self.target.where(**{self.target.__pk__: target_pks}).all()
        if not target_objs:
            return self
        
        target_id_and_obj = { t_obj.get_pk(): t_obj for t_obj in target_objs }

        # 3. set targets to owner
        groups = {}
        for t in through_objs:
            through_fk_on_owner = getattr(t, self.through_fk_on_owner)
            through_fk_on_target = getattr(t, self.through_fk_on_target)
            target = target_id_and_obj.get(through_fk_on_target, None)
            if target:
                groups.setdefault(through_fk_on_owner, []).append(target)

        for o in owner_objs:
            through_fk_on_owner = o.get_pk()
            group = groups.get(through_fk_on_owner, [None])
            o._set_relation_cache(self.name, group[0])

        return self

def has_one(class_, name=None, fk=None, cascade=False,
                    through=None, through_fk_on_owner=None, through_fk_on_target=None):
    if not through:
        r = HasOne(target=class_, name=name, fk=fk, cascade=cascade)
    else:
        r = HasOneThrough(target=class_, name=name, through=through, through_fk_on_owner=through_fk_on_owner, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
