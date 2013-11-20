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
from pyrails.active_record import ActiveRecord
from pyrails.tests.test_association_has_one_helper import *
import unittest


class HasOneTest(unittest.TestCase):

    def setUp(self):
        drop_accounts()
        drop_beneficiaries()

        create_accounts()
        create_beneficiaries()

    def tearDown(self):
        drop_accounts()
        drop_beneficiaries()

    def test_has_one(self):
        self.assertEqual(1, len(Account.association_dict))
        association = Account.association_of('beneficiary')
        self.assertTrue(association.is_has_one())
        self.assertEqual(Beneficiary, association.target)
        self.assertEqual('beneficiary', association.attr_name)
        self.assertEqual('account_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_one_with_some_customer_init_args(self):
        class Account(ActiveRecord): has_one(Beneficiary, attr_name="user")
        self.assertEqual(1, len(Account.association_dict))
        association = Account.association_of('user')
        self.assertTrue(association.is_has_one())
        self.assertEqual(Beneficiary, association.target)
        self.assertEqual('user', association.attr_name)
        self.assertEqual('account_id', association.foreign_key)
        self.assertFalse(association.dependent)

    def test_has_one_with_more_customer_init_args(self):
        class Account(ActiveRecord): has_one(Beneficiary, attr_name="user", foreign_key="user_id", dependent=True)
        self.assertEqual(1, len(Account.association_dict))
        association = Account.association_of('user')
        self.assertTrue(association.is_has_one())
        self.assertEqual(Beneficiary, association.target)
        self.assertEqual('user', association.attr_name)
        self.assertEqual('user_id', association.foreign_key)
        self.assertTrue(association.dependent)
    
    def test_has_one_find(self):
        """ Account#beneficiary (similar to Beneficiary.where(account_id: id).first)
        """
        account = Account(name="account1").save()
        Beneficiary(name="beneficiary1", account_id=account.id).save()
        beneficiary = Account.find(1).beneficiary
        self.assertEqual(1, beneficiary.id)
        self.assertEqual('beneficiary1', beneficiary.name)

    def test_has_one_set_attrbuite(self):
        """ Account#beneficiary=(beneficiary) (similar to beneficiary.account_id = account.id)
        """
        account1 = Account(name="account1").save()
        account2 = Account(name="account2").save()

        beneficiary1 = Beneficiary(name="beneficiary1", account_id=account1.id).save()
        beneficiary2 = Beneficiary(name="beneficiary2", account_id=account2.id).save()

        self.assertEqual(2, Beneficiary.find(2).account_id)

        account = Account.find(1)
        account.beneficiary = beneficiary2

        self.assertEqual(account2.id, beneficiary2.account_id)
        self.assertEqual(2, Beneficiary.find(2).account_id)

    def test_has_one_build(self):
        """ Account#build_beneficiary (similar to Beneficiary(account_id=id))
        """
        account = Account(name="account1").save()
        beneficiary = account.build_beneficiary(name='beneficiary1')
        self.assertEqual(1, beneficiary.account_id)
        self.assertIsNone(beneficiary.id)
        self.assertIsNone(account.beneficiary.id)

    def test_has_one_create(self):
        """ Account#create_beneficiary (similar to b = Beneficiary(account_id=id); b.save(); return b)
        """
        account = Account(name="account1").save()
        account.create_beneficiary(name='beneficiary1')
        beneficiary = Beneficiary.find(1)
        self.assertEqual(1, beneficiary.account_id)
        self.assertEqual(1, beneficiary.id)
        self.assertEqual(1, account.beneficiary.id)


if __name__ == '__main__':
    unittest.main()
