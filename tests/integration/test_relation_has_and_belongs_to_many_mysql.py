#coding: utf8
from sweet.tests import TestCase, Article, Tag


class TestRelationHasAndBelongsToManyMysql(TestCase):
    
    def setUp(self):
        self.remove_record()

    def tearDown(self):
        self.remove_record()

    def remove_record(self):
        Article.delete_all()
        Tag.delete_all()

    # def test_create(self):
    #     u = User.create(name="Jon", age=31)
    #     mobile_id = Mobile.create(name="Nokia", user=u).id
    #     m = Mobile.find(mobile_id)
    #     self.assertEqual(u.id, m.user_id)

    #     u = m.user
    #     self.assertEqual("Jon", u.name)
    #     self.assertEqual(31, u.age)

    


if __name__ == '__main__':
    import unittest
    unittest.main()
