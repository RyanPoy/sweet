#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *


class HasAndBelongsToMany(Relation):
    """owner model belongs to target model
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
    def __init__(self, owner=None, target=None, name=None, fk=None, pk=None):
        self.owner = owner
        self._target_cls_or_target_name = target
        self._name = name
        self._fk = fk
        self._pk = pk

    @property
    def name(self):
        """ owner attribute name
        """
        if not self._name:
            self._name = pythonize(self._get_target_name())
        return self._name

    @property
    def fk(self):
        """ return owner foreign key
        eg. mobile is belongs to user
            fk equals 'user_id', which composition is ： 'user' + '_'+ user.pk
        """
        if not self._fk:
            self._fk = '{target_name}_{target_pk}'.format(
                target_name=pythonize(self._get_target_name()),
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

    def _get_target_name(self):
        """ get the target class name 
        """
        if isinstance(self._target_cls_or_target_name, str):
            name = self._target_cls_or_target_name.split('.')[-1]
        else:
            name = self.target.__name__.split('.')[-1]
        return singularize(name)

    @property
    def target(self):
        """ return target class
        """
        if isinstance(self._target_cls_or_target_name, str):
            self._target_cls_or_target_name = import_object(self._target_cls_or_target_name)
        return self._target_cls_or_target_name

    def inject(self, owner):
        self.owner = owner
        self.owner._register_relation(self.name, self)
        return self

    def get_real_value(self, owner_obj):
        """ eg. if mobile belongs to user.
            User.find(mobile.user_id)
        """
        return self.target.find(getattr(owner_obj, self.fk))


def has_and_belongs_to_many(class_or_classname, name=None, fk=None, pk=None):
    r = HasAndBelongsToMany(target=class_or_classname, name=name, fk=fk, pk=pk)
    relation_q.put(r)
