#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *


class BelongsTo(Relation):
    
    def __init__(self, owner=None, target=None, name=None, fk=None, pk=None):
        """ owner model belongs to target model
        :param owner: model class
        :param target: model class
        :param name: attribute name of owner.
        :param fk: foreign key of owner
        :param pk: primary key of target
        eg. Mobile belongs to User
          owner = Mobile
          target = User
          name = 'user'    # can retrive use Mobile().user
          fk = 'user_id'        # can retrive use Mobile().user_id
          pk = 'id'             # it means that user's priamry key named id
        """
        self.owner = owner
        self._target_cls_or_target_name = target
        self.name = name
        self._fk = fk
        self._pk = pk

    @property
    def fk(self):
        """ return owner foreign key
        eg. mobile is belongs to user
            fk equals 'user_id', which composition is ： 'user' + '_'+ user.pk
        """
        if not self._fk:
            self._fk = '{target_name}_{target_pk}'.format(
                target_name=self.name,
                target_pk=self.target.__pk__
            )
        return self._fk

    @property
    def pk(self):
        """ return target primary key
        eg. mobile is belongs to user
            pk equals 'id', which composition is ：mobile.pk
        """
        return self.target.__pk__

    @property
    def target(self):
        """ return target class
        """
        if isinstance(self._target_cls_or_target_name, str):
            self._target_cls_or_target_name = import_object(self._target_cls_or_target_name)
        return self._target_cls_or_target_name

    def get_real_value(self, owner_obj):
        """ eg. if mobile belongs to user.
            User.find(mobile.user_id)
        """
        return self.target.find(getattr(owner_obj, self.fk))


def belongs_to(name, clazz, fk=None, pk=None):
    r = BelongsTo(target=clazz, name=name, fk=fk, pk=pk)
    relation_q.put(r)
