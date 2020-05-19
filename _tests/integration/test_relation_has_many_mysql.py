#coding: utf8
from sweet._tests import TestCase, User, Mobile
from sweet.orm import Model
from sweet.orm.relations import *
from MySQLdb import IntegrityError


class TestRelationHasManyToMysql(TestCase):
    
    def setUp(self):
        self.remove_record()
 
    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Mobile.delete_all()
        User.delete_all()

    def test_query(self):
        u = User.create(name="Jon", age=31)
        m1 = Mobile.create(name="Nokia", user_id=u.id)
        Mobile.create(name="IPhone", user_id=u.id)

        ms = u.mobiles.all()
        self.assertEqual(2, len(ms))
 
        m = ms[0]
        self.assertEqual(Mobile, type(m))
        self.assertEqual('Nokia', m.name)
        self.assertEqual(u.id, m.user_id)
 
        m = ms[1]
        self.assertEqual(Mobile, type(m))
        self.assertEqual('IPhone', m.name)
        self.assertEqual(u.id, m.user_id)

    def test_query_with_include(self):
        u = User.create(name="Jon", age=31)
        m1 = Mobile.create(name="Nokia", user_id=u.id)
        Mobile.create(name="IPhone", user_id=u.id)

        ms = User.include('mobiles').first().mobiles.all()
        self.assertEqual(2, len(ms))
 
        m = ms[0]
        self.assertEqual(Mobile, type(m))
        self.assertEqual('Nokia', m.name)
        self.assertEqual(u.id, m.user_id)
 
        m = ms[1]
        self.assertEqual(Mobile, type(m))
        self.assertEqual('IPhone', m.name)
        self.assertEqual(u.id, m.user_id)
    
    def test_delete_cascade(self):
        user_id1 = User.create(name="Jon", age=31).id
        Mobile.create(name="Nokia", user_id=user_id1)
        Mobile.create(name="WinPhone", user_id=user_id1)

        user_id2 = User.create(name="Jon", age=31).id
        Mobile.create(name="IPhone", user_id=user_id2)
        Mobile.create(name="Vivo", user_id=user_id2)

        self.assertEqual(4, Mobile.count())
        User.where(id=user_id1).delete()
        self.assertEqual(2, Mobile.count())

        User.find(user_id2).delete()
        self.assertEqual(0, Mobile.count())

    def test_delete_all_cascade(self):
        user_id1 = User.create(name="Jon", age=31).id
        Mobile.create(name="Nokia", user_id=user_id1)

        user_id2 = User.create(name="Jon", age=31).id
        Mobile.create(name="IPhone", user_id=user_id2)

        self.assertEqual(2, Mobile.count())
        User.delete_all()
        self.assertEqual(0, Mobile.count())

    def test_delete_if_set_not_cascade(self):

        class Member(Model):
            __tablename__ = 'users'

            has_many('mobiles', 'sweet._tests.Mobile', cascade=False)
            has_one('car', 'sweet._tests.Car', cascade=False)

        member1 = Member.create(name="Jon", age=31)
        Mobile.create(name="Nokia", user_id=member1.id)

        member2 = Member.create(name="Jon", age=31)
        Mobile.create(name="IPhone", user_id=member2.id)

        with self.assertRaises(IntegrityError) as err:
            member1.delete_all()

        with self.assertRaises(IntegrityError) as err:
            Member.delete_all()


if __name__ == '__main__':
    import unittest
    unittest.main()
