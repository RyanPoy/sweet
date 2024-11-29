#coding: utf8
import re


class AssociateMethod:
    """ for a relation.
    if the relation has a binding method, it can support associate_with_xxxx
    if the relation has a unbinding method, it can support dissociate_with_xxxx
    """
    pattern = re.compile(r'^(di|a)ssociate_with_([_a-zA-Z]\w*)$')

    def __init__(self, model, method_name):
        self.method_name = method_name
        self.model = model

        diss, relation = self.__class__._get_diss_and_relation(model, method_name)
        self.diss = diss
        self.relation = relation

    @classmethod
    def match(cls, model, name):
        if not cls.pattern.match(name):
            return False

        diss, relation = cls._get_diss_and_relation(model, name)
        if not relation:
            return False

        if not diss and not hasattr(relation, 'binding'):
            return False

        if diss and not hasattr(relation, 'unbinding'):
            return False

        return True

    @classmethod
    def _get_diss_and_relation(cls, model, name):
        groups = cls.pattern.match(name).groups()
        diss, relation_name = groups[0], groups[1]
        if relation_name not in model.__relations__:
            return None, None

        diss = True if diss == 'di' else False
        return diss, model.__relations__.get(relation_name)

    def __call__(self, *args):
        if self.diss:
            return self.relation.unbinding(self.model, *args)
        return self.relation.binding(self.model, *args)


class ReloadCacheMethod:
    """ for Model
    """
    pattern = re.compile(r'^reload_([_a-zA-Z]\w*)$')

    def __init__(self, model, method_name):
        self.method_name = method_name
        self.model = model

    @classmethod
    def match(cls, model, name):
        return True if cls.pattern.match(name) else False

    def __call__(self):
        name = self.__class__.pattern.match(self.method_name).groups()[0]
        # return self.model._delete_relation_cache(name)
        return self


class BuildAssociationMethod:
    """ for Model
    """

    pattern = re.compile(r'^build_([_a-zA-Z]\w*)$')

    def __init__(self, model, method_name):
        self.method_name = method_name
        self.model = model

    @classmethod
    def match(cls, model, name):
        return True if cls.pattern.match(name) else False

    def __call__(self):
        name = self.__class__.pattern.match(self.method_name).groups()[0]
        print ('*'*10, name)
        # return self.model._delete_relation_cache(name)
        return self


class MethodMissing:

    methods = [
        AssociateMethod, ReloadCacheMethod, BuildAssociationMethod
    ]

    @classmethod
    def match(cls, model, name):
        for method in cls.methods:
            if method.match(model, name):
                return method
        return None
