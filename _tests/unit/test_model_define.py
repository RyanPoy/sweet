#coding: utf8
from sweet._tests import TestCase
from sweet.orm.model import Model


class ModelAutoStub(Model):
    pass

class ModelCustomerStub(Model):
    __tablename__ = 'customer_models'
    __pk__ = 'customer_pk'
    __timestamp__ = False


# class ModelHasError(Model):
#     __fields__ = {'age': IntField('age', length=10, default=10, null=False)}
#     __pk__ = 'id'


class TestModelDefine(TestCase):

    def test_auto_tablename(self):
        self.assertEqual('model_auto_stubs', ModelAutoStub.__tablename__)

    def test_auto_primary_key(self):
        self.assertEqual('id', ModelAutoStub.__pk__)

    def test_auto_created_at(self):
        self.assertTrue(hasattr(ModelAutoStub, 'created_at'))

    def test_auto_updated_at(self):
        self.assertTrue(hasattr(ModelAutoStub, 'updated_at'))

    def test_customer_tablename(self):
        self.assertEqual('customer_models', ModelCustomerStub.__tablename__)

    def test_customer_primary_key(self):
        self.assertEqual('customer_pk', ModelCustomerStub.__pk__)

    def test_auto_created_at(self):
        self.assertFalse(hasattr(ModelCustomerStub, 'created_at'))

    def test_auto_updated_at(self):
        self.assertFalse(hasattr(ModelCustomerStub, 'updated_at'))


if __name__ == '__main__':
    import unittest
    unittest.main()
