#coding: utf8
import functools
import unittest
import types
from sweet.utils import *


# def add(): pass


# class one(object):

#     def __init__(self, *args):
#         print ('one#__init__ :', *args)
#         # print ('one#__init__ :', fget, type(fget))
#         self.fget = args[0]

#     def __get__(self, owner_self, owner_cls): 
#         print ('one#__get__ :', owner_self, owner_cls)
#         return self.fget(owner_cls)()

#     # def __call__(self, *args, **kwargs):
#     #     print ('one#__call__ :', *args, **kwargs)
#     #     return sum


# class two(object):

#     def __init__(self, *args):
#         print ('two#__init__ :', *args)
#         # print ('two#__init__ :', fget, type(fget))
#         # self.fget = fget

#     def __get__(self, owner_self, owner_cls): 
#         print ('two#__get__ :', owner_self, owner_cls)
#         return self.fget(owner_cls)

#     def __call__(self, func, *args, **kwargs):
#         print ('two#__call__ :', self, func, args, kwargs)
#         return func(self)


class both(object):

    def __init__(self, fk=None, pk=None, through=None):
        print ("*"*10, "__init__")
        self.func = None
        self.fk = fk
        self.pk = pk
        self.through = through

        if callable(fk):
            self.func = fk
            self.fk = None
        else:
            self.func = None

    def __get__(self, owner_self, owner_cls):
        """ without argument should be call 
        """
        if owner_self is None:
            return self
        return self.func(owner_self)()

    def __call__(self, func=None):
        """ with argument should be call
        """
        print("*"*10, "__call__")
        if func is None:
            raise TypeError("'%s' object is not callable" % self.__class__.__name__)
        self.func = func
        return self


class Phone(object):
    pass


class User(object):

    @both
    def phone(self):
        return Phone

    @both("id")
    def mobile(self):
        print ('User#mobile : two : ', self)
        return Phone


class TestFilter(unittest.TestCase):

    def test_111(self):
        # U = User
        # u = U()

        # print( U.phone )
        # print( u.phone )
        
        # print( U.mobile )
        # print( u.mobile )
        pass


if __name__ == '__main__':
    import unittest
    unittest.main()
