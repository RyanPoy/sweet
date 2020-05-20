#coding: utf8
from sweet.orm.relations.relation import Relation, relation_q
from sweet.orm.relations.has_many_through import HasManyThrough
from sweet.utils.collection import Collection
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
        self._name = name
        self._through_fk_one_owner = through_fk_on_owner
        self._through_table = through_table
        self._through_fk_on_target = through_fk_on_target

    @property
    def name(self):
        if not self._name:
            self._name = pythonize(pluralize(self.target_name))
        return self._name

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

    def delete_all_real_value(self, owner_objs):
        """ eg. article has and belongs many tags
            1) Article.delete_all() # should be delete all mobiles which belongs to users

            2) a = Article.first()
               a.delete()  # should be delete mobiles which belongs to u
        """
        pks = [ o.get_pk() for o in owner_objs ]
        if pks:
            owner_objs[0].__class__.db.records(self.through_table).where(**{self.through_fk_on_owner: pks}).delete()
        return self

    def preload(self, owner_objs):
        owner_pks = list(set([ o.get_pk() for o in owner_objs ]))
        if not owner_pks:
            return self

        # 1. found the through objects
        db = self.owner.db
        through_objs = db.records(self.through_table).where(**{self.through_fk_on_owner: owner_pks}).all()
        if not through_objs:
            return self

        # 2. found the target objects
        target_pks = [ getattr(t, self.through_fk_on_target) for t in through_objs ]
        if not target_pks:
            return self

        target_objs = self.target.find(*target_pks)
        if not target_objs:
            return self
        
        target_id_and_obj = { t_obj.get_pk(): t_obj for t_obj in target_objs }

        # 3. set targets to owner
        groups = {}
        for t in through_objs:
            through_fk_on_owner = getattr(t, self.through_fk_on_owner)
            through_fk_on_target = getattr(t, self.through_fk_on_target)
            target = target_id_and_obj.get(through_fk_on_target, None)
            if target:
                groups.setdefault(through_fk_on_owner, []).append(target)

        for o in owner_objs:
            through_fk_on_owner = o.get_pk()
            group = groups.get(through_fk_on_owner, [])
            o._set_relation_cache(self.name, Collection(*group))

        return self

    def binding(self, model1, *model2s):
        if model2s:
            # 1. check all model2 has been persisted
            for m in model2s:
                if not m.persisted():
                    raise model2.HasNotBeenPersisted()

            # 2. get model2 which has been binded on model1
            db = model1.__class__.db
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
            db = model1.__class__.db
            db.records(self.through_table) \
              .where(**{self.through_fk_on_owner : model1.get_pk()}) \
              .where(**{self.through_fk_on_target: pks }) \
              .delete()
        return self


def has_and_belongs_to_many(class_, name=None, through_table=None, through_fk_on_owner=None, through_fk_on_target=None):
    r = HasAndBelongsToMany(target=class_, name=name, through_fk_on_owner=through_fk_on_owner, through_table=through_table, through_fk_on_target=through_fk_on_target)
    relation_q.put(r)
