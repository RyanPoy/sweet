# -*- coding:utf-8 -*-
from sweet.criteria import JoinClause
from sweet.utils import *


class Relation(object):
    
    __relations__ = []
    
    def __init__(self, target_class, owner_class=None, owner_attr=None, foreign_key=None):
        """
        target_class: 目标的 class
        owner_class: 拥有者的class
        foreign_key: 目标的 class 所在table的字段名
        """
        self._owner = owner_class
        self._target = target_class
        self._foreign_key = foreign_key
        self._owner_attr = owner_attr
    
    @classmethod
    def push(cls, relation):
        cls.__relations__.append(relation)
    
    @classmethod
    def iter(cls):
        while cls.__relations__:
            yield cls.__relations__.pop()

    @property
    def owner(self):
        return self._owner

    @property
    def target(self):
        """_target is None, should be return None
           _target is a active record class path, should be parse it and set a active record class to _target, then return it
           _target is a active record class, should be return it   
        """
        if self._target is None:
            return None
        if isinstance(self._target, str):
            self._target = import_object(self._target)
        return self._target
    
    @property
    def owner_attr(self):
        if self._owner_attr:
            return self._owner_attr
        if self.owner is None:
            return None
        from record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        target = self.target
        if target is None:
            return None
        self._owner_attr = singularize(target.table_name)
        return self._owner_attr

    @property
    def target_pk_column(self):
        target = self.target
        return target.__pk__ if target else None

    @property
    def foreign_key(self):
        """Phone belongs to User
        owner_column must be user_id
        """
        if self._foreign_key:
            return self._foreign_key
        if self.owner is None:
            return None
        from record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        owner = self.owner
        if owner is None:
            return None
        foreign_key = singularize(owner.table_name) + '_id'
        if not self.target.has_column(foreign_key):
            raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key

    def __get__(self, instance, owner):
        foreign_key_value = getattr(instance, instance.__pk__)
        return self.target.where(**{self.foreign_key: foreign_key_value}).first()

    def inject(self, owner=None):
        if owner: self._owner = owner
        self._owner.__relations__.append(self)
        setattr(self._owner, self.owner_attr, relation(self))


class relation(object):
    """ decorator
    """
    def __init__(self, r):
        self.r = r
        
    def __get__(self, instance, owner):
        return self.r._get(instance, owner)


############# shared methods ############
def owner_attr_for_has_one_and_has_belongs_to(self):
    """ User has one Phone, owner attr must be 'phone'
    Phone belongs to User, owner attr must be 'user'
    """
    if self._owner_attr:
        return self._owner_attr
    if self.owner is None:
        return None
    from sweet.record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
#     target = self.target
#     if target is None:
#         return None
    if isinstance(self._target, str):
        name = self._target.split('.')[-1]
    elif issubclass(self._target, ActiveRecord):
        name = self._target.__name__
    else:
        return None
    self._owner_attr = singularize(pythonize(name))
    return self._owner_attr


def owner_attr_for_has_many(self):
    """ User has many Phone, owner attr must be 'phones'
    """
    if self._owner_attr:
        return self._owner_attr
    if self.owner is None:
        return None
    from sweet.record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
#     target = self.target
#     if target is None:
#         return None
#     self._owner_attr = pluralize(pythonize(target.__name__))
    if isinstance(self._target, str):
        name = self._target.split('.')[-1]
    elif issubclass(self._target, ActiveRecord):
        name = self._target.__name__
    else:
        return None
    self._owner_attr = pluralize(pythonize(name))
    return self._owner_attr


def foreign_key_for_belongs_to(self):
    """ Phone belongs to User, foreign key must be user_id, and appear in Phone
    """
    if self._foreign_key:
        return self._foreign_key
    if self.owner is None:
        return None
    from sweet.record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    target = self.target
    if target is None:
        return None
    foreign_key = pythonize(target.__name__) + '_id'
    if not self.owner.has_column(foreign_key):
        raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.owner.__name__))
    self._foreign_key = foreign_key
    return self._foreign_key


def foreign_key_for_has_one_and_has_many(self):
    """ User has one Phone, foreign key must be user_id, and appear in Phone
    User has many Phone, foreign key must be user_id, and appear in Phone
    """
    if self._foreign_key:
        return self._foreign_key
    owner = self.owner
    if owner is None:
        return None
    from sweet.record import ActiveRecord # lazy import
    if not issubclass(self.owner, ActiveRecord):
        return None
    foreign_key = singularize(pythonize(owner.__name__)) + '_id'
    if not self.target.has_column(foreign_key):
        raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
    self._foreign_key = foreign_key
    return self._foreign_key


############# belongs_to #############
class BelongsTo(Relation):

    owner_attr = property(owner_attr_for_has_one_and_has_belongs_to)
    foreign_key = property(foreign_key_for_belongs_to)
    
    def _get(self, instance, owner):
        foreign_key_value = getattr(instance, self.foreign_key)
        return self.target.where(**{self.target_pk_column: foreign_key_value}).first()
    
    def _delete(self, owner_instance):
        pass

        
def belongs_to(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = BelongsTo(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation


############# has_one ###############
class HasOne(Relation):
    
    owner_attr = property(owner_attr_for_has_one_and_has_belongs_to)
    foreign_key = property(foreign_key_for_has_one_and_has_many)

    def _get(self, instance, owner):
        return self.target.where(**{self.foreign_key: instance.pk}).first()

    def _delete(self, owner_instance):
        return self.target.where(**{self.foreign_key: owner_instance.pk}).delete()


def has_one(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasOne(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation


############# has_many ###############
class HasMany(Relation):
    owner_attr = property(owner_attr_for_has_many)
    foreign_key = property(foreign_key_for_has_one_and_has_many)

    def _get(self, instance, owner):
        foreign_key_value = getattr(instance, instance.__pk__)
        return self.target.where(**{self.foreign_key: foreign_key_value}).all()


def has_many(target_class_or_classpath, foreign_key=None, owner_attr=""):
    relation = HasMany(target_class=target_class_or_classpath, foreign_key=foreign_key, owner_attr=owner_attr)
    Relation.push(relation)
    return relation


################ has_and_belongs_to_many ###########
class HasAndBelongsToMany(Relation):
    
    def __init__(self, target_class, owner_class=None, owner_attr=None, 
                 foreign_key=None, target_foreign_key=None, association_table=None):
        super(HasAndBelongsToMany, self).__init__(target_class, owner_class, owner_attr, foreign_key)
        self._association_table = association_table
        self._target_foreign_key = target_foreign_key

    owner_attr = property(owner_attr_for_has_many)

    @property
    def target_foreign_key(self):
        if self._target_foreign_key:
            return self._target_foreign_key
        target = self.target
        if target is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(target, ActiveRecord):
            return None
        target_foreign_key = singularize(pythonize(target.__name__)) + '_id'
#         if not self.target.has_column(foreign_key):
#             raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._target_foreign_key = target_foreign_key
        return self._target_foreign_key

    @property
    def foreign_key(self):
        """ User has one Phone, foreign key must be user_id, and appear in Phone
        User has many Phone, foreign key must be user_id, and appear in Phone
        """
        if self._foreign_key:
            return self._foreign_key
        owner = self.owner
        if owner is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord):
            return None
        
        foreign_key = singularize(pythonize(owner.__name__)) + '_id'
#         if not self.target.has_column(foreign_key):
#             raise ColumnNotInColumns('"%s" not in %s columns' % (foreign_key, self.target.__name__))
        self._foreign_key = foreign_key
        return self._foreign_key
    
    @property
    def association_table(self):
        if self._association_table:
            return self._association_table
        owner, target = self.owner, self.target
        if owner is None or target is None:
            return None
        from sweet.record import ActiveRecord # lazy import
        if not issubclass(self.owner, ActiveRecord) or not issubclass(self.target, ActiveRecord):
            return None
        l = [ tableize(self.owner.__name__), tableize(self.target.__name__) ]
        l.sort()
        self._association_table = '_'.join(l) 
        return self._association_table

    def _get(self, instance, owner):
        foreign_key_value = getattr(instance, instance.__pk__)

        return self.target.join(
            JoinClause(self.association_table)\
                .on('`%s`.`%s` = ?' % (self.association_table, self.foreign_key), foreign_key_value)\
                .on('`%s`.`%s` = `%s`.`%s`' % (self.association_table, self.target_foreign_key, self.target.table_name, self.target.__pk__))
        ).all()


def has_and_belongs_to_many(target_class_or_classpath, foreign_key=None, target_foreign_key=None, 
                                owner_attr=None, association_table=None):
    relation = HasAndBelongsToMany(target_class=target_class_or_classpath, foreign_key=foreign_key, 
                    target_foreign_key=target_foreign_key, owner_attr=owner_attr, association_table=association_table)
    Relation.push(relation)
    return relation
