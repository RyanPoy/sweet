#coding: utf8
from sweet.tests import TestCase, User
from sweet.database.recordset import MySQLRecordset
from sweet.database import MySQL
from sweet.utils import Collection, mydict


class ModelCRUDMySQLTest(TestCase):
    
    def setUp(self):
        self.db = MySQL('sweet_test')
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        User.delete_all()

    def test_create(self):
        u = User.create(name='jack', age=25)
        self.assertTrue(u.id is not None)
        self.assertTrue('jack', u.name)
        self.assertTrue(25, u.age)

    def test_save_successful_when_model_has_not_been_persisted(self):
        self.assertEqual(0, User.count())
        u = User(name='jack', age=25)
        self.assertTrue(u.id is None)
        u.save()
        self.assertTrue(u.id is not None)
        self.assertEqual(1, User.count())

    def test_save_will_call_update_when_model_has_been_persisted(self):
        self.assertEqual(0, User.count())
        u = User.create(name='jack', age=25)
        _id = u.id
        u.name = 'lily'
        u.save()
        self.assertEqual(_id, u.id)
        self.assertEqual('lily', u.name)
        self.assertEqual(25, u.age)
        self.assertEqual(1, User.count())

    def test_update(self):
        u = User.create(name='jack', age=25)
        u.update(name='lily', age=30)
        self.assertEqual('lily', u.name)
        self.assertEqual(30, u.age)
        u_from_db = User.find(u.id)
        self.assertEqual('lily', u_from_db.name)
        self.assertEqual(30, u_from_db.age)

    def test_update_all(self):
        User.create(name='jack', age=25)
        User.create(name='jon', age=30)
        User.update_all(name='unkown', age=15)
        us = User.all()
        u = us[0]
        self.assertEqual('unkown', u.name)
        self.assertEqual(15, u.age)
        u = us[1]
        self.assertEqual('unkown', u.name)
        self.assertEqual(15, u.age)

    def test_delete(self):
        u = User.create(name='jack', age=25)
        u.delete()
        self.assertEqual(0, User.count())

    def test_delete_all(self):
        User.create(name='jack', age=25)
        User.create(name='jon', age=30)
        User.delete_all()
        self.assertEqual(0, User.count())

    # def test_first(self):
    #     r = self.db.records('users').first()
    #     self.assertEqual(1, r.id)
    #     self.assertEqual('jack', r.name)
    #     self.assertEqual(25, r.age)

    # def test_last(self):
    #     r = self.db.records('users').last()
    #     self.assertEqual(4, r.id)
    #     self.assertEqual('lily', r.name)
    #     self.assertEqual(26, r.age)

    # def test_all(self):
    #     coll = self.db.records('users').all()
    #     self.assertTrue(isinstance(coll, Collection))
    #     self.assertEqual(4, len(coll))
        
    #     self.assertEqual(1, coll[0].id)
    #     self.assertEqual('jack', coll[0].name)
    #     self.assertEqual(25, coll[0].age)

    #     self.assertEqual(2, coll[1].id)
    #     self.assertEqual('lucy', coll[1].name)
    #     self.assertEqual(22, coll[1].age)

    # def test_count(self):
    #     r = self.db.records('users').count()
    #     self.assertEqual(4, r)

    # def test_count_with_column(self):
    #     r = self.db.records('users').count('age')
    #     self.assertEqual(4, r)

    # def test_max(self):
    #     r = self.db.records('users').max('age')
    #     self.assertEqual(27, r)

    # def test_min(self):
    #     r = self.db.records('users').min('age')
    #     self.assertEqual(22, r)

    # def test_avg(self):
    #     r = self.db.records('users').avg('age')
    #     self.assertEqual(25, r)

    # def test_sum(self):
    #     r = self.db.records('users').sum('age')
    #     self.assertEqual(100, r)

    # def test_exists(self):
    #     r = self.db.records('users').where(age=27).exists()
    #     self.assertTrue(r)

    # def test_not_exists(self):
    #     r = self.db.records('users').where(age=100).exists()
    #     self.assertFalse(r)

    # def test_select(self):
    #     coll = self.db.records('users').select('name').where(age=22).all()
    #     self.assertEqual(1, len(coll))
    #     r = coll[0]

    #     self.assertEqual(1, len(r.keys()))
    #     self.assertTrue('name' in r.keys())
    #     self.assertFalse('age' in r.keys())
    #     self.assertEqual('lucy', r.name)

    # def test_select_with_multiple_columns(self):
    #     coll = self.db.records('users').select('name', 'age').where(age=22).all()
    #     self.assertEqual(1, len(coll))
    #     r = coll[0]
    #     self.assertEqual(2, len(r.keys()))
    #     self.assertTrue('age' in r.keys())
    #     self.assertTrue('name' in r.keys())
    #     self.assertEqual('lucy', r.name)
    #     self.assertEqual(22, r.age)

    # def test_select_twice(self):
    #     coll = self.db.records('users').select('name').select('age').where(age=22).all()
    #     self.assertEqual(1, len(coll))
    #     r = coll[0]
    #     self.assertEqual(2, len(r.keys()))
    #     self.assertTrue('age' in r.keys())
    #     self.assertTrue('name' in r.keys())
    #     self.assertEqual('lucy', r.name)
    #     self.assertEqual(22, r.age)

    # def test_where(self):
    #     coll = self.db.records('users').where(age=22).all()
    #     self.assertEqual(1, len(coll))
    #     r = coll[0]
    #     self.assertEqual(2, r.id)
    #     self.assertEqual('lucy', r.name)
    #     self.assertEqual(22, r.age)

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

    # def test_cross_join(self):
    #     coll = self.db.records('users').cross_join('mobiles', on="users.id=mobiles.user_id").where(mobiles__name="iphone").all()
    #     self.assertEqual(2, len(coll))
    #     self.assertEqual(1, coll[0].id)
    #     self.assertEqual(2, coll[1].id)

    # def test_insert_getid(self):
    #     tb = self.db.records('users')
    #     user_id = tb.insert_getid(id=300, name="Poy", age=33)
    #     self.assertEqual(300, user_id)

    #     rs = tb.where(id=300).all()
    #     self.assertEqual(1, len(rs))

    #     r = rs[0]
    #     self.assertEqual(300, r.id)
    #     self.assertEqual('Poy', r.name)
    #     self.assertEqual(33, r.age)

    # def test_insert_getid_with_autoincrementid(self):
    #     tb = self.db.records('users')
    #     user_id = tb.insert_getid(name="Poy", age=33)

    #     rs = tb.where(name="Poy").where(age=33).all()
    #     self.assertEqual(1, len(rs))

    #     r = rs[0]
    #     self.assertEqual(user_id, r.id)
    #     self.assertEqual('Poy', r.name)
    #     self.assertEqual(33, r.age)

    # def test_insert(self):
    #     tb = self.db.records('users')
    #     cnt = tb.insert(id=300, name="Poy", age=33)
    #     self.assertEqual(1, cnt)
    #     rs = tb.where(id=300).all()
    #     self.assertEqual(1, len(rs))

    #     r = rs[0]
    #     self.assertEqual(300, r.id)
    #     self.assertEqual('Poy', r.name)
    #     self.assertEqual(33, r.age)

    # def test_multple_insert(self):
    #     tb = self.db.records('users')
    #     tb.insert([
    #         dict(id=300, name="Poy", age=33),
    #         dict(id=400, name="Ryan", age=44),
    #     ])
    #     rs = tb.where(age__gt=30).all()
    #     self.assertEqual(2, len(rs))

    #     self.assertEqual(300, rs[0].id)
    #     self.assertEqual('Poy', rs[0].name)
    #     self.assertEqual(33, rs[0].age)

    #     self.assertEqual(400, rs[1].id)
    #     self.assertEqual('Ryan', rs[1].name)
    #     self.assertEqual(44, rs[1].age)

    # def test_delete(self):
    #     tb = self.db.records('mobiles')
    #     r = tb.where(id=[1, 3]).delete()
    #     c = tb.where(id=[1, 2, 3]).all()
    #     self.assertEqual(2, r)
    #     self.assertEqual(1, len(c))
    #     self.assertEqual(2, c[0].id)

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

    # def test_update(self):
    #     c = self.db.records('mobiles').where(name='iphone').all()
    #     self.assertEqual(2, len(c))

    #     r = self.db.records("mobiles").where(name='iphone').update(name='aphone')
    #     self.assertEqual(2, r)

    #     c = self.db.records('mobiles').where(name='iphone').all()
    #     self.assertEqual(0, len(c))
        
    #     c = self.db.records('mobiles').where(name='aphone').all()
    #     self.assertEqual(2, len(c))

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
