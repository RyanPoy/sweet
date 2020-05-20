#coding: utf8
import functools
from sweet.orm import Transaction, Model
from sweet._tests import *


def p(*msg):
    print ("*"*10, *msg)

class Atomic(object):

    def __init__(self, func_or_db=None):
        print("__init__")
        if callable(func_or_db):
            self.func = func_or_db
            self._db = None
        else:
            self.func = None
            self._db = func_or_db

    @property
    def db(self):
        return self._db or Model.db
    
    def __get__(self, instance, class_):
        print("__get__")
        def _(*args, **kwargs):
            t = None
            try:
                t = Transaction(self.db)
                t.begin()
                relt = self.func(instance, *args, **kwargs)
                t.commit()
                return relt
            except:
                if t is not None:
                    t.rollback()
                raise
        return _

    def __call__(self, func=None):

        def _(*args, **kwargs):
            t = None
            try:
                t = Transaction(self.db)
                t.begin()
                relt = self.func(instance, *args, **kwargs)
                t.commit()
                return relt
            except:
                if t is not None:
                    t.rollback()
                raise

        self.func = func
        return _


class Foo(object):

    def get_value(self, value):

        @Atomic
        # @Atomic()
        # @Atomic("*******")
        def _(value):
            print (self, value)

        return _

f = Foo()
f.get_value("123")

# @Atomic
# @Atomic()
# @Atomic("*******")
# def pp(value):
#     print (value)

# pp("123")