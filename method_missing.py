#coding: utf8
import re


class FindMethodMissing(object):

    __find_by_pattern = re.compile('^find_(all_by|by)_([_a-zA-Z]\w*)$')
    __find_or_by_pattern = re.compile('^find_or_(init|create)_by_([_a-zA-Z]\w*)$')

    def __init__(self, instance, method_name):
        self.__name = method_name
        self.__model_instance = instance

    @classmethod
    def match(cls, name):
        return cls.__find_by_pattern.match(name) or cls.__find_or_by_pattern.match(name)

    def __call__(self, *args):
        def extract_scope_or_method_and_arguments(match, args):
            groups = match.groups()
            scope = groups[0]
            args_name = groups[1].split('_and_')
            return scope, dict(zip(args_name, args))

        r = None
        match = self.__class__.__find_by_pattern.match(self.__name)
        if match:
            scope, argments = extract_scope_or_method_and_arguments(match, args)
            if scope == 'all_by':
                r = self.__model_instance.where(**argments).all
            else: # must 'by'
                r = self.__model_instance.where(**argments).first
        else:
            match = self.__class__.__find_or_by_pattern.match(self.__name)
            if match:
                scope, argments = extract_scope_or_method_and_arguments(match, args)
                if scope == 'create':
                    r = self.__model_instance.create(**argments)
                else: # must 'init'
                    r = self.__model_instance(**argments)
        return r


class CreateOrBuildMethodMissing(object):

    __pattern = re.compile('^(create|build)_([_a-zA-Z]\w*)$')

    def __init__(self, instance, method_name):
        self.__name = method_name
        self.__model_instance = instance

    @classmethod
    def match(cls, name):
        return cls.__pattern.match(name)
            
    def __call__(self, *args, **kwargs):
        r = None
        match = self.__class__.__pattern.match(self.__name)
        if match:
            scope, association_propert_name = match.groups()
            association = self.__model_instance.association_dict.get(association_propert_name, None)
            if not association:
                raise AttributeError("'%s' object has no attribute '%s'" % (self.__model_instance.__class__.__name__, self.__name))
            foreign_key = association.foreign_key
            if foreign_key not in kwargs:
                kwargs[foreign_key] = self.__model_instance.id
            if scope == 'create':
                r = association.target.create(*args, **kwargs)
            else: # must 'build'
                r = association.target(*args, **kwargs)
            setattr(self.__model_instance, association_propert_name, r)
        return r
