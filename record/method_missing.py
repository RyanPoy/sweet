#coding: utf8
import re

class MethodMissing(object):

    FIND_BY_P = re.compile('^find_(all_by|by)_([_a-zA-Z]\w*)$')
    FIND_OR_BY_P = re.compile('^find_or_(init|create)_by_([_a-zA-Z]\w*)$')

    def __init__(self, instance, method_name):
        self.name = method_name
        self.record = instance

    @classmethod
    def match(cls, name):
        return cls.FIND_BY_P.match(name) or cls.FIND_OR_BY_P.match(name)

    def __call__(self, *args):
        def extract_scope_or_method_and_arguments(match, args):
            groups = match.groups()
            scope = groups[0]
            args_name = groups[1].split('_and_')
            return scope, dict(zip(args_name, args))

        r = None
        match = self.FIND_BY_P.match(self.name)
        if match:
            scope, argments = extract_scope_or_method_and_arguments(match, args)
            if scope == 'all_by':
                r = self.record.where(**argments).all()
            else: # must 'by'
                r = self.record.where(**argments).first()
        else:
            match = self.FIND_OR_BY_P.match(self.name)
            if match:
                scope, argments = extract_scope_or_method_and_arguments(match, args)
                r = self.record.where(**argments).first()
                if r is None:
                    if scope == 'create':
                        r = self.record.create(**argments)
                    else: # must 'init'
                        r = self.record(**argments)
        return r

# class CreateOrBuildMethodMissing(object):
# 
#     __pattern = re.compile('^(create|build)_([_a-zA-Z]\w*)$')
# 
#     def __init__(self, instance, method_name):
#         self.name = method_name
#         self.record = instance
# 
#     @classmethod
#     def match(cls, name):
#         return cls.__pattern.match(name)
#             
#     def __call__(self, *args, **kwargs):
#         r = None
#         match = self.__class__.__pattern.match(self.name)
#         if match:
#             scope, association_propert_name = match.groups()
#             association = self.record.association_of(association_propert_name) # belongs_to or has_one
#             if not association: #or association.is_belongs_to():
#                 raise AttributeError("'%s' object has no attribute '%s'" % (self.record.__class__.__name__, self.name))
#             foreign_key = association.foreign_key
#             if foreign_key not in kwargs:
#                 kwargs[foreign_key] = self.record.id
#             if scope == 'create':
#                 r = association.target.create(*args, **kwargs)
#             else: # must 'build'
#                 r = association.target(*args, **kwargs)
#             setattr(self.record, association_propert_name, r)
#         return r
