#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.orm.relations.has_many_through import HasManyThrough
from sweet.utils.inflection import *
from sweet.utils import *


class HasAndBelongsToMany(HasManyThrough):
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
    def __init__(self, owner=None, target=None, name=None, through_table=None, through_fk_on_owner=None, through_fk_on_target=None):
        self.owner = owner
        self._target_cls_or_target_name = target
        self.name = name
        self._through_fk_one_owner = through_fk_on_owner
        self._through_table = through_table
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
    def through_table(self):
        if not self._through_table:
            self._through_table = '_'.join(sorted([self.owner.__tablename__, self.target.__tablename__]))
        return self._through_table

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
            owner_objs[0].__class__._new_db().records(self.through_table).where(**{self.through_fk_on_owner: pks}).delete()
        return self

    def binding(self, model1, *model2s):
        if model2s:
            # 1. check all model2 has been persisted
            for m in model2s:
                if not m.persisted():
                    raise model2.ModelHasNotBeenPersisted()

            # 2. get model2 which has been binded on model1
            db = model1.__class__._new_db()
            model2s_binded = db.records(self.through_table) \
                               .where(**{self.through_fk_on_owner : model1.get_pk()}) \
                               .where(**{self.through_fk_on_target: [ m2.get_pk() for m2 in model2s ]}) \
                               .all()
            model2_ids_binded = set([ getattr(e, self.through_fk_on_target) for e in model2s_binded ])

            # 3. insert record which has not been binding in through table
            record_dict_list = [
                {
                    self.through_fk_on_target: m2.get_pk(),
                    self.through_fk_on_owner: model1.get_pk()
                } for m2 in model2s if m2.get_pk() not in model2_ids_binded
            ]
            if record_dict_list:
                db.records(self.through_table).insert(records=record_dict_list)

        return self

    def unbinding(self, model1, *model2s):
        pks = [ m2.get_pk() for m2 in model2s if m2.persisted() ]
        if pks:
            db = model1.__class__._new_db()
            db.records(self.through_table) \
              .where(**{self.through_fk_on_owner : model1.get_pk()}) \
              .where(**{self.through_fk_on_target: pks }) \
              .delete()
        return self


def has_and_belongs_to_many(name, clazz, through_table=None, through_fk_on_owner=None, through_fk_on_target=None):
    r = HasAndBelongsToMany(target=clazz, name=name, through_fk_on_owner=through_fk_on_owner, through_table=through_table, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
