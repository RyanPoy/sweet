#coding: utf8
from sweet.orm.relations.relation import relation_q
from sweet.orm.relations.has_many import HasMany
from sweet.utils.inflection import *


class HasOne(HasMany):

    def _get_target_name(self):
        """ get the target class name 
        """
        if isinstance(self._target_cls_or_target_name, str):
            name = self._target_cls_or_target_name.split('.')[-1]
        else:
            name = self.target.__name__.split('.')[-1]
        return singularize(name)

    def get_real_value(self, owner_obj):
        """ eg. user has one car
            Car.where(user_id=user.id)
        """
        return self.target.where(**{self.fk: owner_obj.get_pk()}).first()

    def delete_all_real_value(self, owner_objs):
        """ eg. user has one car
            1) User.delete_all() # should be delete all cars which belongs to users

            2) u = User.first()
               u.delete()  # should be delete car which belongs to u
        """
        pks = [ o.get_pk() for o in owner_objs ]
        if pks:
            self.target.delete_all(**{self.fk: pks})
        return self


def has_one(class_or_name, attr_name=None, fk=None, pk=None):
    r = HasOne(target=class_or_name, attr_name=attr_name, fk=fk, pk=pk)
    relation_q.put(r)

