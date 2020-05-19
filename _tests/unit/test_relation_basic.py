#coding: utf8
from sweet._tests import TestCase
from sweet.orm.model import Model
from sweet.orm.relations import *
import inspect


class TestRelationBasic(TestCase):

    def test_belongs_to_without_argument(self):

        class Member(Model):
            __tablename__ = 'users'

        class Phone(Model):
            __tablename__ = 'mobiles'
            belongs_to(Member)

        r = Phone.__relations__.get('member')
        self.assertEqual(BelongsTo, type(r))
        self.assertEqual(Phone, r.owner)
        self.assertEqual(Member, r.target)
        self.assertEqual('member_id', r.owner_fk)
        self.assertEqual('member', r.name)
        
    def test_belongs_to_with_argument(self):

        class Member(Model):
            __pk__ = 'member_id'
            __tablename__ = 'users'

        class Phone(Model):
            __tablename__ = 'mobiles'
            belongs_to(Member, name='user', fk='owner_id')

        r = Phone.__relations__.get('user')
        self.assertEqual(BelongsTo, type(r))
        self.assertEqual(Phone, r.owner)
        self.assertEqual(Member, r.target)
        self.assertEqual('owner_id', r.owner_fk)
        self.assertEqual('user', r.name)

    def test_has_many_without_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_many(Phone)

        r = Member.__relations__.get('phones')
        self.assertEqual(HasMany, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('member_id', r.target_fk)
        self.assertEqual('phones', r.name)

    def test_has_many_with_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_many(Phone, name='mobiles', fk='user_id')

        r = Member.__relations__.get('mobiles')
        self.assertEqual(HasMany, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('user_id', r.target_fk)
        self.assertEqual('mobiles', r.name)

    def test_has_one_without_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_one(Phone)

        r = Member.__relations__.get('phone')
        self.assertEqual(HasOne, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('member_id', r.target_fk)
        self.assertEqual('phone', r.name)

    def test_has_one_with_argument(self):

        class Phone(Model):
            __tablename__ = 'mobiles'
        
        class Member(Model):
            __tablename__ = 'users'
            has_one(Phone, name='mobile', fk='user_id')

        r = Member.__relations__.get('mobile')
        self.assertEqual(HasOne, type(r))

        self.assertEqual(Member, r.owner)
        self.assertEqual(Phone, r.target)
        self.assertEqual('user_id', r.target_fk)
        self.assertEqual('mobile', r.name)

    def test_has_and_belongs_to_many_without_argument(self):

        class Role(Model):
            pass

        class Group(Model):
            has_and_belongs_to_many(Role)

        HasAndBelongsToMany(target=Group).set_owner(Role)

        r = Role.__relations__.get('groups')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Role, r.owner)
        self.assertEqual(Group, r.target)
        self.assertEqual('role_id', r.through_fk_on_owner)
        self.assertEqual('groups', r.name)
        self.assertEqual('groups_roles', r.through_table)
        self.assertEqual('group_id', r.through_fk_on_target)

        r = Group.__relations__.get('roles')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Group, r.owner)
        self.assertEqual(Role, r.target)
        self.assertEqual('group_id', r.through_fk_on_owner)
        self.assertEqual('roles', r.name)
        self.assertEqual('groups_roles', r.through_table)
        self.assertEqual('role_id', r.through_fk_on_target)

    def test_has_and_belongs_to_many_with_argument(self):

        class Role(Model):
            pass

        class Group(Model):
            has_and_belongs_to_many(Role, name='roles', through_fk_on_owner="gid", through_fk_on_target="rid")

        HasAndBelongsToMany(target=Group, name='groups', through_fk_on_owner="rid", through_fk_on_target="gid").set_owner(Role)

        r = Role.__relations__.get('groups')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Role, r.owner)
        self.assertEqual(Group, r.target)
        self.assertEqual('rid', r.through_fk_on_owner)
        self.assertEqual('groups', r.name)
        self.assertEqual('groups_roles', r.through_table)
        self.assertEqual('gid', r.through_fk_on_target)

        r = Group.__relations__.get('roles')
        self.assertEqual(HasAndBelongsToMany, type(r))
        self.assertEqual(Group, r.owner)
        self.assertEqual(Role, r.target)
        self.assertEqual('gid', r.through_fk_on_owner)
        self.assertEqual('roles', r.name)
        self.assertEqual('groups_roles', r.through_table)
        self.assertEqual('rid', r.through_fk_on_target)


if __name__ == '__main__':
    import unittest
    unittest.main()

