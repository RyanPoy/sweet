#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *


class BelongsTo(Relation):
    
    def __init__(self, owner=None, target=None, name=None, fk=None):
        """ owner model belongs to target model
        :param owner: model class
        :param target: model class
        :param name: attribute name of owner.
        :param fk: foreign key of owner
        :param pk: primary key of target
        eg. Mobile belongs to User
          owner = Mobile
          target = User
          name = 'user'         # can retrive use Mobile().user
          fk = 'user_id'        # can retrive use Mobile().user_id
        """
        self.owner = owner
        self._target_cls_or_target_name = target
        self._name = name
        self._owner_fk = fk

    @property
    def name(self):
        if not self._name:
            self._name = pythonize(singularize(self.target_name))
        return self._name

    @property
    def owner_fk(self):
        """ return owner foreign key
        eg. mobile is belongs to user
            fk equals 'user_id', which composition is ： 'user' + '_'+ user.pk
        """
        if not self._owner_fk:
            self._owner_fk = '{target_name}_{target_pk}'.format(
                target_name=self.name,
                target_pk=self.target.__pk__
            )
        return self._owner_fk

    def get_real_value(self, owner_obj):
        """ eg. if mobile belongs to user.
            User.find(mobile.user_id)
        """
        return self.target.find(getattr(owner_obj, self.owner_fk))

    def preload(self, owner_objs):
        target_pks = list(set([ getattr(o, self.owner_fk) for o in owner_objs ]))
        if target_pks:
            target_objs = { t.get_pk(): t for t in self.target.where(**{ self.target.__pk__ : target_pks}).all() }
            for o in owner_objs:
                fk = getattr(o, self.owner_fk)
                setattr(o, self.name, target_objs.get(fk, None))

        return self

    def inject(self, owner_model, target_model):
        attr_name = self.owner_fk
        setattr(owner_model, attr_name, target_model.get_pk())


def belongs_to(class_, name=None, fk=None):
    r = BelongsTo(target=class_, name=name, fk=fk)
    relation_q.put(r)
