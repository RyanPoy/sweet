#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *
from sweet.utils import *


class HasAndBelongsToMany(Relation):
    """owner model belongs to target model
    :param owner: model class
    :param target: model class
    :param name: attribute name of owner.
    :param fk: foreign key of owner
    :param pk: primary key of target
    eg. Article has and belongs to many Tag
      owner = Article
      target = Tag
      name = 'tags'         # can retrive use Article().tags
      fk = 'article_id'     # can retrive use Article().user_id
      pk = 'id'             # it means that user's priamry key named id
    """
    def __init__(self, owner=None, target=None, name=None, fk=None, pk=None, through=None):
        self.owner = owner
        self._target_cls_or_target_name = target
        self._name = name
        self._fk = fk
        self._pk = pk
        self._through = through

    @property
    def fk(self):
        """ return througt table foreign key for owner
        eg. Article is has many and belongs to many User
            fk equals 'article_id', which composition is ： 'article' + '_'+ article.pk
        """
        if not self._fk:
            self._fk = '{owner_name}_{owner_pk}'.format(
                owner_name=self._get_owner_name(),
                owner_pk=self.owner.__pk__
            )
        return self._fk

    @property
    def pk(self):
        """ return owner primary key
        eg. Article is has many and belongs to many User
            pk equals 'id', which composition is ：article.pk
        """
        return self.owner.__pk__

    @property
    def through_table(self):
        if not hasattr(self, '_through_table'):
            if self._through:
                self._through_table = self._through.__tablename__
            else:
                self._through_table = self._get_through_table()
        return self._through_table

    def _get_through_table(self):
        owner_table_name = self.owner.__tablename__
        target_table_name = self.target.__tablename__
        return '_'.join(sorted([owner_table_name, target_table_name]))

    def _get_target_name(self):
        """ get the target class name 
        """
        if isinstance(self._target_cls_or_target_name, str):
            name = self._target_cls_or_target_name.split('.')[-1]
        else:
            name = self.target.__name__.split('.')[-1]
        return pluralize(name)

    def _get_owner_name(self):
        return pythonize(singularize(self.owner.__name__.split('.')[-1]))

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


def has_and_belongs_to_many(class_or_classname, name=None, fk=None, pk=None, through=None):
    r = HasAndBelongsToMany(target=class_or_classname, name=name, fk=fk, pk=pk, through=through)
    relation_q.put(r)
