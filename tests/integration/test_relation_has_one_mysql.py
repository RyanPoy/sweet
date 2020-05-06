#coding: utf8
from sweet.tests import TestCase, User, Mobile, Car


class TestHasOneToMysql(TestCase):
    
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
        Car.create(name="Mazda", user_id=u.id)

        c = u.car
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


if __name__ == '__main__':
    import unittest
    unittest.main()
