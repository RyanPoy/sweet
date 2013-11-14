# -*- coding:utf-8 -*-
from pyrails.sql_builder import SQLBuilder
from pyrails.record import ActiveRecord
from pyrails.associations import has_one
import unittest


class SQLBuilderTest(unittest.TestCase):

    def test_select_some_columns(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).select('id', 'name', 'age').delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.id, users.name, users.age FROM users', sql)
        self.assertEqual([], params)

    def test_select_all_columns(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).select('*').delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users', sql)
        self.assertEqual([], params)

        sql, params = SQLBuilder(User).delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users', sql)
        self.assertEqual([], params)

    def test_where_with_a_string_condition(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).where("name='pengyi' and type=1").delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE name='pengyi' and type=1", sql)
        self.assertEqual([], params)

    def test_where_with_a_array_condition(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).where("name=? and type=?", 'pengyi', 1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE name=? and type=?", sql)
        self.assertEquals(['pengyi', 1], params)

    def test_where_with_a_hash_which_has_a_array_value(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).where(name=['pengyi', 'poy'], type=1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.type = ? AND users.name in (?, ?)", sql)
        self.assertEquals([1, 'pengyi', 'poy'], params)
        
    def test_where_with_a_kwargs(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).where(name=['pengyi', 'poy'], type=1).limit(10, 1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.type = ? AND users.name in (?, ?) LIMIT 10 OFFSET 1", sql)
        self.assertEquals([1, 'pengyi', 'poy'], params)
        
    def test_where_with_chain(self):
        class User(ActiveRecord):
            pass
        sql, params = SQLBuilder(User).where(name=['pengyi', 'poy']).where(type=1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)

    def test_complex_chain(self):
        class User(ActiveRecord): pass
        sql, params = SQLBuilder(User).where(name=['pengyi', 'poy']).where(type=1) \
                                      .order('name DESC').group('name') \
                                      .having(age=10).limit(10, 1) \
                                      .delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.name in (?, ?) AND users.type = ? GROUP BY users.name HAVING users.age = ? ORDER BY name DESC LIMIT 10 OFFSET 1", sql)
        self.assertEquals(['pengyi', 'poy', 1, 10], params)

    def test_func(self):
        class User(ActiveRecord): pass
        c = SQLBuilder(User).where(name=['pengyi', 'poy']).where(type=1)
        c._func = ('count', '*')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual("SELECT COUNT(*) AS count FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)
        
        c = SQLBuilder(User).where(name=['pengyi', 'poy']).where(type=1)
        c._func = ('sum', 'type')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual("SELECT SUM(type) AS sum FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)

    def test_joins_with_str(self):
        class User(ActiveRecord): pass
        c = SQLBuilder(User).where(name=['pengyi', 'poy']).joins('INNER JOIN orders ON orders.user_id = user.id')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users INNER JOIN orders ON orders.user_id = user.id AND users.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_joins(self):
        class Post(ActiveRecord): pass
        class User(ActiveRecord): has_one(Post)

        c = SQLBuilder(User).where(name=['pengyi', 'poy']).joins('post')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users INNER JOIN posts ON posts.user_id = users.id AND users.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
