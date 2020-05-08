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

    @property
    def name(self):
        """ return owner attribute name
        """
        if not self._name:
            self._name = pythonize(self._get_target_name())
        return self._name

    def get_real_value(self, owner_obj):
        pass

    def delete_all_real_value(self, owner_objs):
        pass

    def _get_target_name(self):
        pass
