#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.utils.inflection import *
from sweet.utils import *


class HasManyThrough(Relation):
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
    """
    def __init__(self, owner=None, target=None, name=None, through=None, through_fk_on_owner=None, through_fk_on_target=None):
        self.owner = owner
        self._target_cls_or_target_name = target
        self.name = name
        self._through_fk_one_owner = through_fk_on_owner
        self._through_cls_or_through_name = through
        self._through_fk_on_target = through_fk_on_target

    @property
    def through_fk_on_owner(self):
        """ return througt table foreign key for owner
        eg. Article is has many and belongs to many User
            fk equals 'article_id', which composition is ： 'article' + '_'+ article.pk
        """
        if not self._through_fk_one_owner:
            self._through_fk_one_owner = '{owner_name}_{owner_pk}'.format(
                owner_name=pythonize(singularize(self.owner.__name__)),
                owner_pk=self.owner.__pk__
            )
        return self._through_fk_one_owner

    @property
    def through_fk_on_target(self):
        if not self._through_fk_on_target:
            self._through_fk_on_target = '{target_name}_{target_pk}'.format(
                target_name=pythonize(singularize(self.target.__name__)),
                target_pk=self.target.__pk__
            ) 
        return self._through_fk_on_target

    @property
    def through(self):
        """ return through class
        """
        if isinstance(self._through_cls_or_through_name, str):
            self._through_cls_or_through_name = import_object(self._through_cls_or_through_name)
        return self._through_cls_or_through_name

    @property
    def through_table(self):
        return self.through.__tablename__

    def get_real_value(self, owner_obj):
        """ eg. if mobile belongs to user.
            User.find(mobile.user_id)
        """
        target_and_through_table_join_on = '{target_table}.{target_pk}={through_table}.{through_fk_on_target}'.format(
            target_table=self.target.__tablename__,
            target_pk=self.target.__pk__,
            through_table=self.through_table,
            through_fk_on_target=self.through_fk_on_target
        )

        owner_and_through_table_join_on = '{owner_table}.{owner_pk}={through_table}.{through_fk_on_owner}'.format(
            owner_table=self.owner.__tablename__,
            owner_pk=self.owner.__pk__,
            through_table=self.through_table,
            through_fk_on_owner=self.through_fk_on_owner
        )

        return self.target.objects \
                          .join(self.through_table, on=target_and_through_table_join_on) \
                          .join(self.owner.__tablename__, on=owner_and_through_table_join_on) \
                          .where(**{self.through_table+'__'+self.through_fk_on_owner: owner_obj.get_pk()})

    def delete_all_real_value(self, owner_objs):
        """ eg. article has and belongs many tags
            1) Article.delete_all() # should be delete all mobiles which belongs to users

            2) a = Article.first()
               a.delete()  # should be delete mobiles which belongs to u
        """
        pks = [ o.get_pk() for o in owner_objs ]
        if pks:
            self.through.delete_all(**{self.through_fk_on_owner:pks})
        return self


def has_many(name, clazz, fk=None, cascade=False):
    r = HasMany(target=clazz, name=name, fk=fk, cascade=cascade)
    relation_q.put(r)


def has_many_through(name, clazz, through=None, through_fk_on_owner=None, through_fk_on_target=None):
    r = HasManyThrough(target=clazz, name=name, through=through, through_fk_on_owner=through_fk_on_owner, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
