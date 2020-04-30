#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *


class BelongsTo(Relation):
    """owner model belongs to target model
    :param owner: model class
    :param target: model class
    :param attr_name: attribute name of owner.
    :param fk: foreign key of owner
    :param pk: primary key of target
    eg. Mobile belongs to User
      owner = Mobile
      target = User
      attr_name = user    # can retrive use Mobile.user
      fk = user_id        # can retrive use Mobile.user_id
      pk = id             # it means that user's priamry key named id
    """
    def __init__(self, owner=None, target=None, attr_name=None, fk=None, pk=None):
        self.owner = owner
        self._target_cls_or_target_name = target
        self._attr_name = attr_name
        self._fk = fk
        self._pk = pk

    @property
    def attr_name(self):
        if not self._attr_name:
            self._attr_name = pythonize(self._get_target_name()).split('.')[-1]
        return self._attr_name

    @property
    def fk(self):
        if not self._fk:
            self._fk = '{target_name}_{target_pk}'.format(
                target_name=pythonize(self._get_target_name()).split('.')[-1],
                target_pk=self.target.__pk__
            )
        return self._fk

    @property
    def pk(self):
        return self.target.__pk__

    def _get_target_name(self):
        """ get the target class name 
        """
        if isinstance(self._target_cls_or_target_name, str):
            return self._target_cls_or_target_name.split('.')[-1],
        return self.target.__name__.split('.')[-1],

    @property
    def target(self):
        if isinstance(self._target_cls_or_target_name, str):
            self._target_cls_or_target_name = import_object(self._target_cls_or_target_name)
        return self._target_cls_or_target_name

    def inject(self, owner):
        self.owner = owner
        self.owner._register_relation(self.attr_name, self)
        return self

    def get_real_value(self, owner_obj):
        """ eg. if mobile belongs to user.
            User.find(mobile.user_id)
        """
        return self.target.find(getattr(owner_obj, self.fk))


def belongs_to(class_or_name, attr_name=None, fk=None, pk=None):
    r = BelongsTo(target=class_or_name, attr_name=attr_name, fk=fk, pk=pk)
    relation_q.put(r)
