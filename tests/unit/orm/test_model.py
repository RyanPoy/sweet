#coding: utf8
from sweet.tests.unit import TestCase
from sweet.orm.model import Model


class ModelAutoStub(Model):
    pass

class ModelCustomerStub(Model):
    __tbname__ = 'customer_models'
    __pk__ = 'customer_pk'


class ModelTest(TestCase):

    def test_auto_tablename(self):
        self.assertEqual('model_auto_stubs', ModelAutoStub.__tbname__)

    def test_customer_tablename(self):
        self.assertEqual('customer_models', ModelCustomerStub.__tbname__)

    def test_auto_primary_key(self):
        self.assertEqual('id', ModelAutoStub.__pk__)

    def test_customer_primary_key(self):
        self.assertEqual('customer_pk', ModelCustomerStub.__pk__)


if __name__ == '__main__':
    import unittest
    unitest.testmain()
