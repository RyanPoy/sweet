#coding: utf8
from sweet.database import MySQL, DatabaseManager
from sweet.tests.integrations import blog
from sweet.record import ActiveRecord
from blog import User
import unittest


class BlogMySQL(unittest.TestCase):
    
    def setUp(self):
        db = MySQL('test', user='root', password='', show_sql=True)
        db.execute(blog.drop_db_sql)
        db.execute(blog.create_db_sql)
        db.close()

        db = MySQL(blog.database, user='root', password='', show_sql=True)
        db.execute(blog.create_table_sql)
        db.close()

        ActiveRecord.__dbmanager__ = DatabaseManager({
            'driver': 'mysql',
            'host': 'localhost',
            'port': 3306,
            'database': blog.database,
            'user': 'root',
            'password': '',
            'show_sql': True,
        })

    def tearDown(self):
        db = MySQL('test', user='root', password='', show_sql=True)
        db.execute(blog.drop_db_sql)
        db.close()
        ActiveRecord.__dbmanager__.close_all_connection()

    def test_business(self):
        User.create(name='admin', password="123123") # register ok
        self.assertEquals(None, User.where(name="admin", password="err_password").first()) # login with err password should be return None
        admin = User.where(name="admin", password="123123").first() # login ok
        self.assertTrue(isinstance(admin, User))


if __name__ == '__main__':
    unittest.main()
