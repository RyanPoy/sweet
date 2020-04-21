#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
import unittest


class TestCase(unittest.TestCase):
    
    pass


class UserForTest(object):

    def __init__(self, name, age):
        self.name = name
        self.age = age

