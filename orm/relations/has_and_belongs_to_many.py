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
        self.name = name
        self._fk = fk
        self._pk = pk
        self._through = through
        self._target_fk = None

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
    def target_pk(self):
        return self.target.__pk__

    @property
    def target_fk(self):
        if not self._target_fk:
            self._target_fk = '{target_name}_{target_pk}'.format(
                target_name=self._get_target_name(),
                target_pk=self.target_pk
            ) 
        return self._target_fk

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

    def _get_owner_name(self):
        return pythonize(singularize(self.owner.__name__.split('.')[-1]))

    def _get_target_name(self):
        return pythonize(singularize(self.target.__name__.split('.')[-1]))

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
        target_and_throught_table_join_on = '{target_table}.{target_pk}={through_table}.{through_fk}'.format(
            target_table=self.target.__tablename__,
            target_pk=self.target.__pk__,
            through_table=self.through_table,
            through_fk=self.target_fk
        )

        owner_and_throught_table_join_on = '{owner_table}.{owner_pk}={through_table}.{through_fk}'.format(
            owner_table=self.owner.__tablename__,
            owner_pk=self.owner.__pk__,
            through_table=self.through_table,
            through_fk=self.fk
        )

        return self.target.objects \
                          .join(self.through_table, on=target_and_throught_table_join_on) \
                          .join(self.owner.__tablename__, on=owner_and_throught_table_join_on) \
                          .where(**{self.through_table+'__'+self.fk: owner_obj.get_pk()})

    def delete_all_real_value(self, owner_objs):
        """ eg. article has and belongs many tags
            1) Article.delete_all() # should be delete all mobiles which belongs to users

            2) a = Article.first()
               a.delete()  # should be delete mobiles which belongs to u
        """
        pks = [ o.get_pk() for o in owner_objs ]
        if pks:
            owner_objs[0].__class__._new_db().records(self.through_table).where(**{self.fk: pks}).delete()
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
                               .where(**{self.fk : model1.get_pk()}) \
                               .where(**{self.target_fk: [ m2.get_pk() for m2 in model2s ]}) \
                               .all()
            model2_ids_binded = set([ getattr(e, self.target_fk) for e in model2s_binded ])

            # 3. insert record which has not been binding in through table
            record_dict_list = [
                {
                    self.target_fk: m2.get_pk(),
                    self.fk: model1.get_pk()
                } for m2 in model2s if m2.get_pk() not in model2_ids_binded
            ]
            if record_dict_list:
                db.records(self.through_table).insert(records=record_dict_list)

        return self

    def unbinding(self, model1, *model2s):
        if model2s:
            # 1. check all model2 has been persisted
            for m in model2s:
                if not m.persisted():
                    raise model2.ModelHasNotBeenPersisted()

            db = model1.__class__._new_db()
            model2s_binded = db.records(self.through_table) \
                               .where(**{self.fk : model1.get_pk()}) \
                               .where(**{self.target_fk: [ m2.get_pk() for m2 in model2s ]}) \
                               .delete()
        return self


def has_and_belongs_to_many(name, clazz, fk=None, pk=None, through=None):
    r = HasAndBelongsToMany(target=clazz, name=name, fk=fk, pk=pk, through=through)
    relation_q.put(r)
