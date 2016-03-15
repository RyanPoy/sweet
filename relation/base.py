#coding: utf8
from sweet.utils import import_object

class Relation(object):
    
    __relations__ = []
    
    def __init__(self, target_class, owner_class=None, owner_attr=None, foreign_key=None):
        """
        target_class: 目标的 class
        owner_class: 拥有者的class
        foreign_key: 目标的 class 所在table的字段名
        """
        self.owner = owner_class
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
    def target_pk_column(self):
        target = self.target
        return target.__pk__ if target else None

    def inject(self, owner=None):
        if owner: self.owner = owner
        self.owner.__relations__.append(self)
        setattr(self.owner, self.owner_attr, self)
