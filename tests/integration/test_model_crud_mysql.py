#coding: utf8
from sweet.tests import TestCase, User
from sweet.database.recordset import MySQLRecordset
from sweet.database import MySQL
from sweet.utils import mydict


class TestModelCRUDMySQL(TestCase):
    
    def setUp(self):
        self.db = MySQL('sweet_test')
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        User.delete_all()

    # def test_create(self):
    #     u = User.create(name='jack', age=25)
    #     self.assertTrue(u.id is not None)
    #     self.assertTrue('jack', u.name)
    #     self.assertTrue(25, u.age)

    # def test_create_all(self):
    #     self.assertEqual(3, User.create_all(
    #         dict(name='jack', age=25),
    #         dict(name='jon', age=30),
    #         dict(name='Lucy', age=3)
    #     ))
    #     self.assertEqual(3, User.count())

    # def test_save_successful_when_model_has_not_been_persisted(self):
    #     self.assertEqual(0, User.count())
    #     u = User(name='jack', age=25)
    #     self.assertTrue(u.id is None)
    #     u.save()
    #     self.assertTrue(u.id is not None)
    #     self.assertEqual(1, User.count())

    # def test_save_will_call_update_when_model_has_been_persisted(self):
    #     self.assertEqual(0, User.count())
    #     u = User.create(name='jack', age=25)
    #     _id = u.id
    #     u.name = 'lily'
    #     u.save()
    #     self.assertEqual(_id, u.id)
    #     self.assertEqual('lily', u.name)
    #     self.assertEqual(25, u.age)
    #     self.assertEqual(1, User.count())

    # def test_update(self):
    #     u = User.create(name='jack', age=25)
    #     u.update(name='lily', age=30)
    #     self.assertEqual('lily', u.name)
    #     self.assertEqual(30, u.age)
    #     u_from_db = User.find(u.id)
    #     self.assertEqual('lily', u_from_db.name)
    #     self.assertEqual(30, u_from_db.age)

    # def test_update_all(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.update_all(name='unkown', age=15)
    #     us = User.all()
    #     u = us[0]
    #     self.assertEqual('unkown', u.name)
    #     self.assertEqual(15, u.age)
    #     u = us[1]
    #     self.assertEqual('unkown', u.name)
    #     self.assertEqual(15, u.age)

    # def test_delete(self):
    #     u = User.create(name='jack', age=25)
    #     u.delete()
    #     self.assertEqual(0, User.count())

    # def test_delete_all(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.delete_all()
    #     self.assertEqual(0, User.count())

    def test_delete_all_with_attrs(self):
        User.create(name='jack', age=25)
        User.create(name='jon', age=30)
        User.create(name='Lucy', age=35)
        User.create(name='Lily', age=40)
        self.assertEqual(4, User.count())

        User.delete_all(age__gte=30)
        self.assertEqual(1, User.count())
        u = User.where(name='jack', age=25)
        self.assertTrue(u is not None)

    # def test_first(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.create(name='Lucy', age=35)
    #     u = User.first()
    #     self.assertEqual('jack', u.name)
    #     self.assertEqual(25, u.age)

    # def test_last(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.create(name='Lucy', age=35)
    #     u = User.last()
    #     self.assertEqual('Lucy', u.name)
    #     self.assertEqual(35, u.age)

    # def test_all(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.create(name='Lucy', age=35)
    #     us = User.all()
    #     self.assertEqual(3, len(us))
    #     self.assertEqual(25, us[0].age)
    #     self.assertEqual(30, us[1].age)
    #     self.assertEqual(35, us[2].age)

    # def test_count(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.create(name='Lucy', age=35)
    #     self.assertEqual(3, User.count())
    #     self.assertEqual(3, User.count('name'))

    # def test_max(self):
    #     User.create(name='jack', age=25)
    #     User.create(name='jon', age=30)
    #     User.create(name='Lucy', age=35)
    #     self.assertEqual(35, User.max('age'))

    # def test_min(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     self.assertEqual(25, User.min('age'))

    # def test_avg(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     self.assertEqual(30, User.avg('age'))

    # def test_sum(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     self.assertEqual(90, User.sum('age'))

    # def test_exists(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     self.assertTrue(User.where(age=30).exists())

    # def test_not_exists(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     self.assertFalse(User.where(age=27).exists())

    # def test_select(self):
    #     User.create(name='jon', age=30)
    #     User.create(name='jack', age=25)
    #     User.create(name='Lucy', age=35)
    #     us = User.select('name').where(age=25).all()
    #     self.assertEqual(1, len(us))

    # def test_where(self):
    #     u = User.create(name='jon', age=30)
    #     us = User.where(name='jon').all()
    #     self.assertEqual(1, len(us))
    #     self.assertEqual(u.id, us[0].id)
    #     self.assertEqual('jon', us[0].name)
    #     self.assertEqual(30, us[0].age)

    # def test_find(self):
    #     ids = [ 
    #         User.create(name='jon', age=30).id,
    #         User.create(name='jack', age=25).id,
    #         User.create(name='Lucy', age=35).id
    #     ]
    #     us = User.find(*ids)
    #     self.assertEqual(3, len(us))
    #     self.assertEqual(User, type(us[0]))
    #     self.assertEqual(ids[0], us[0].id)
    #     self.assertEqual(ids[1], us[1].id)
    #     self.assertEqual(ids[2], us[2].id)

    # def test_find_or_create(self):
    #     self.assertEqual(0, User.count())
    #     u = User.find_or_create(name='jon', age=30)
    #     self.assertEqual(1, User.count())
    #     self.assertEqual(u.id, User.first().id)

    # def test_join(self):
    #     coll = self.db.records('users').join('mobiles', on="users.id=mobiles.user_id").where(mobiles__name="iphone").all()
    #     self.assertEqual(2, len(coll))
    #     self.assertEqual(1, coll[0].id)
    #     self.assertEqual(2, coll[1].id)
    #     self.assertEqual(1, coll[0]['mobiles.id'])
    #     self.assertEqual(3, coll[1]['mobiles.id'])

    # def test_left_join(self):
    #     coll = self.db.records('users').left_join('mobiles', on="users.id=mobiles.user_id").where(mobiles__name="iphone").all()
    #     self.assertEqual(2, len(coll))
    #     self.assertEqual(1, coll[0].id)
    #     self.assertEqual(2, coll[1].id)

    # def test_right_join(self):
    #     coll = self.db.records('users').left_join('mobiles', on="users.id=mobiles.user_id").where(mobiles__name="iphone").all()
    #     self.assertEqual(2, len(coll))
    #     self.assertEqual(1, coll[0].id)
    #     self.assertEqual(2, coll[1].id)

    # def test_delete_with_join(self):
    #     r = self.db.records('mobiles').join('users', on="users.id=mobiles.user_id").where(users__id=1).delete()
    #     c = self.db.records('mobiles').all()
    #     self.assertEqual(2, r)
    #     self.assertEqual(1, len(c))
    #     self.assertEqual(3, c[0].id)

    # def test_truncate(self):
    #     r = self.db.records('mobiles').truncate()
    #     c = self.db.records('mobiles').all()
    #     self.assertEqual(0, len(c))

    # def test_update_with_join(self):
    #     u = self.db.records('users').where(age=25).first()
    #     self.assertTrue(u is not None)

    #     r = self.db.records('users').join('mobiles', on="users.id=mobiles.user_id").where(mobiles__name='xiaomi').update(age=52)
    #     self.assertEqual(1, r)
    
    #     u = self.db.records('users').where(age=25).first()
    #     self.assertTrue(u is None)

    #     u = self.db.records('users').where(age=52).first()
    #     self.assertTrue(u is not None)
    #     self.assertEqual(1, u.id)


if __name__ == '__main__':
    import unittest
    unittest.main()
