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


def has_one(class_or_classname, name=None, fk=None, pk=None, cascade=False):
    r = HasOne(target=class_or_classname, name=name, fk=fk, pk=pk, cascade=cascade)
    relation_q.put(r)
