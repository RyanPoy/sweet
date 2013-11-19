#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
import re
from pyrails.active_support import Inflection


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
        from pyrails.active_record import Association
        r = None
        match = self.__class__.__pattern.match(self.__name)
        if match:
            scope, association_propert_name = match.groups()
            association = self.__model_instance.association_of(association_propert_name) # belongs_to or has_one
            if not association:
                association = self.__model_instance.association_of(Inflection.pluralize(association_propert_name)) # has_many
            if not association: #or association.is_belongs_to():
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
