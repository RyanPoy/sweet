#coding: utf8

class classproperty(object):
    """
    A decorate for class method. Use it like this:
    
    class Foo(object):
    
        __value = 10
        
        @classproperty
        def value(cls):
            cls.__value += 1
            return cls.__value
    
    for x in range(10):
        print Foo.value
    """
    def __init__(self, fget):
        self.fget = fget

    def __get__(self, owner_self, owner_cls):
        return self.fget(owner_cls)