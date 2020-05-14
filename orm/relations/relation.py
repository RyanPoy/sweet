#coding: utf8
from collections import OrderedDict as oDict
from queue import Queue as Q
from sweet.utils.inflection import *


class RelationQ(Q):
    def get(self, block=True, timeout=None):
        if self.qsize() <= 0:
            return None
        return super().get(block=block, timeout=timeout)
relation_q = RelationQ()


class Relation(object):

    def get_real_value(self, owner_obj):
        pass

    def delete_all_real_value(self, owner_objs):
        pass

    def set_owner(self, owner):
        self.owner = owner
        self.owner._register_relation(self.name, self)
        return self
