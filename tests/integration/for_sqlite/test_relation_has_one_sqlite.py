#coding: utf8
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', '..'))

import unittest
from tests.integration.for_sqlite.helper import User, Mobile, Car


class TestHasOneToSQLite(unittest.TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Car.delete_all()
        User.delete_all()

    def test_query(self):
        u = User.create(name="Jon", age=31)
        Car.create(name="Benz", user_id=u.id)

        c = User.first().car
        self.assertEqual(Car, type(c))
        self.assertEqual('Benz', c.name)
        self.assertEqual(u.id, c.user_id)

    def test_query_with_include(self):
        u = User.create(name="Jon", age=31)
        Car.create(name="Benz", user_id=u.id)

        c = User.include('car').first().car
        self.assertEqual(Car, type(c))
        self.assertEqual('Benz', c.name)
        self.assertEqual(u.id, c.user_id)

    def test_create(self):
        u = User.create(name="Jon", age=31)
        car_id = Car.create(name="Benz", user=u).id
        c = Car.find(car_id)
        self.assertEqual(u.id, c.user_id)

        u = c.user
        self.assertEqual("Jon", u.name)
        self.assertEqual(31, u.age)

    def test_save(self):
        u = User.create(name="Jon", age=31)
        car_id = Car(name="Benz", user=u).save().id

        c = Car.find(car_id)
        self.assertEqual(u.id, c.user_id)

        u = c.user
        self.assertEqual("Jon", u.name)
        self.assertEqual(31, u.age)

    def test_update(self):
        u1 = User.create(name="Jon", age=31)
        u2 = User.create(name="Lily", age=21)
        c = Car(name="Benz", user=u1).save()
        self.assertEqual(u1.id, c.user_id)

        c.update(user=u2)
        self.assertEqual(u2.id, c.user_id)
        c = Car.where(name='Benz').first()
        self.assertEqual(u2.id, c.user_id)

    def test_delete_cascade(self):
        user_id1 = User.create(name="Jon", age=31).id
        Car.create(name="Benz", user_id=user_id1)

        user_id2 = User.create(name="Jon", age=31).id
        Car.create(name="Mazda", user_id=user_id2)

        self.assertEqual(2, Car.count())
        User.where(id=user_id1).delete()
        self.assertEqual(1, Car.count())

        User.find(user_id2).delete()
        self.assertEqual(0, Car.count())

    def test_delete_all_cascade(self):
        user_id1 = User.create(name="Jon", age=31).id
        Car.create(name="Benz", user_id=user_id1)

        user_id2 = User.create(name="Jon", age=31).id
        Car.create(name="Mazda", user_id=user_id2)

        self.assertEqual(2, Car.count())
        User.delete_all()
        self.assertEqual(0, Car.count())


if __name__ == '__main__':
    unittest.main()
