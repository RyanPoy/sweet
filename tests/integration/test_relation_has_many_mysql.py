#coding: utf8
from sweet.tests import TestCase, User, Mobile


class TestHasManyToMysql(TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Mobile.delete_all()
        User.delete_all()

    def test_query(self):
        u = User.create(name="Jon", age=31)
        Mobile.create(name="Nokia", user_id=u.id)
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



if __name__ == '__main__':
    import unittest
    unittest.main()
