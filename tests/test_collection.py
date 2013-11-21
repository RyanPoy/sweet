#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from pyrails.activerecord import Collection, ActiveRecord, has_one, belongs_to
import unittest


class CollectionTest(unittest.TestCase):

    def test_select_some_columns(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).select('id', 'name', 'age').delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.id, users.name, users.age FROM users', sql)
        self.assertEqual([], params)

    def test_select_all_columns(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).select('*').delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users', sql)
        self.assertEqual([], params)

        sql, params = Collection(User).delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users', sql)
        self.assertEqual([], params)

    def test_where_with_a_string_condition(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where("name='pengyi' and type=1").delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE name='pengyi' and type=1", sql)
        self.assertEqual([], params)

    def test_where_with_a_array_condition(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where("name=? and type=?", 'pengyi', 1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE name=? and type=?", sql)
        self.assertEquals(['pengyi', 1], params)

    def test_where_with_a_hash_which_has_a_array_value(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where(name=['pengyi', 'poy'], type=1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.type = ? AND users.name in (?, ?)", sql)
        self.assertEquals([1, 'pengyi', 'poy'], params)
        
    def test_where_with_a_kwargs(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where(name=['pengyi', 'poy'], type=1).limit(10, 1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.type = ? AND users.name in (?, ?) LIMIT 10 OFFSET 1", sql)
        self.assertEquals([1, 'pengyi', 'poy'], params)
        
    def test_where_when_like_query(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where('name like "?abc%" AND type = ?', 1).limit(10, 1).delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users WHERE name like "?abc%" AND type = ? LIMIT 10 OFFSET 1', sql)
        self.assertEquals([1], params)

    def test_where_with_chain(self):
        class User(ActiveRecord):
            pass
        sql, params = Collection(User).where(name=['pengyi', 'poy']).where(type=1).delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)

    def test_complex_chain(self):
        class User(ActiveRecord): pass
        sql, params = Collection(User).where(name=['pengyi', 'poy']).where(type=1) \
                                      .order('name DESC').group('name') \
                                      .having(age=10).limit(10, 1) \
                                      .delete_or_update_or_find_sql()
        self.assertEqual("SELECT users.* FROM users WHERE users.name in (?, ?) AND users.type = ? GROUP BY users.name HAVING users.age = ? ORDER BY name DESC LIMIT 10 OFFSET 1", sql)
        self.assertEquals(['pengyi', 'poy', 1, 10], params)

    def test_func(self):
        class User(ActiveRecord): pass
        c = Collection(User).where(name=['pengyi', 'poy']).where(type=1)
        c._func = ('count', '*')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual("SELECT COUNT(*) AS count FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)
        
        c = Collection(User).where(name=['pengyi', 'poy']).where(type=1)
        c._func = ('sum', 'type')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual("SELECT SUM(type) AS sum FROM users WHERE users.name in (?, ?) AND users.type = ?", sql)
        self.assertEquals(['pengyi', 'poy', 1], params)

    def test_joins_with_str(self):
        class User(ActiveRecord): pass
        c = Collection(User).where(name=['pengyi', 'poy']).joins('INNER JOIN orders ON orders.user_id = user.id')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT users.* FROM users INNER JOIN orders ON orders.user_id = user.id AND users.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_has_one_joins(self):
        class Post(ActiveRecord): 
            __column_names__ = ['id', 'title', 'content', 'user_id']
        class User(ActiveRecord): 
            __column_names__ = ['id', 'name', 'password']
            has_one(Post)

        c = Collection(User).where(name=['pengyi', 'poy']).joins('post')
        sql, params = c.delete_or_update_or_find_sql()
        #self.assertEqual('SELECT t0.id AS t0_r0, t0.name AS t0_r1, t0.password AS t0_r2, t1.id as t1_r0, t1.title AS t1_r1, t1.content AS t1_r2, t1.user_id AS t1_r3 FROM users AS t0 INNER JOIN posts as t1 ON t1.user_id = t0.id AND t0.name in (?, ?)', sql)
        self.assertEqual('SELECT users.* FROM users INNER JOIN posts ON posts.user_id = users.id AND users.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_belongs_to_joins(self):
        class User(ActiveRecord): pass
        class Post(ActiveRecord): belongs_to(User)

        c = Collection(Post).where(name=['pengyi', 'poy']).joins('user')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN users ON users.id = posts.user_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_customer_fkname_belongs_to_joins(self):
        class User(ActiveRecord): pass
        class Post(ActiveRecord): belongs_to(User, foreign_key='author_id')

        c = Collection(Post).where(name=['pengyi', 'poy']).joins('user')
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN users ON users.id = posts.author_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_single_nested_blongs_to_joins(self):
        class Father(ActiveRecord): pass
        class User(ActiveRecord): belongs_to(Father)
        class Post(ActiveRecord): belongs_to(User)
        c = Collection(Post).where(name=['pengyi', 'poy']).joins({'user': 'father'})
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN users ON users.id = posts.user_id INNER JOIN fathers ON fathers.id = users.father_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_multi_nested_blongs_to_joins(self):
        class Company(ActiveRecord): pass
        class Father(ActiveRecord): belongs_to(Company)
        class User(ActiveRecord): belongs_to(Father)
        class Post(ActiveRecord): belongs_to(User)
        c = Collection(Post).where(name=['pengyi', 'poy']).joins({'user': {'father': 'company'}})
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN users ON users.id = posts.user_id INNER JOIN fathers ON fathers.id = users.father_id INNER JOIN companies ON companies.id = fathers.company_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_complex_blongs_to_joins(self):    
        class Company(ActiveRecord): pass
        class Father(ActiveRecord): belongs_to(Company)
        class User(ActiveRecord): belongs_to(Father)
        class Post(ActiveRecord): 
            belongs_to(User)
            belongs_to(Father)
            belongs_to(Company)
        c = Collection(Post).where(name=['pengyi', 'poy']).joins('company', {'father': 'company'}, {'user': {'father': 'company'}})
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN companies ON companies.id = posts.company_id INNER JOIN fathers ON fathers.id = posts.father_id INNER JOIN companies ON companies.id = fathers.company_id INNER JOIN users ON users.id = posts.user_id INNER JOIN fathers ON fathers.id = users.father_id INNER JOIN companies ON companies.id = fathers.company_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    def test_associations_multi_nested_and_level_blongs_to_joins(self):
        class Company(ActiveRecord): pass
        class Father(ActiveRecord): belongs_to(Company)
        class User(ActiveRecord): 
            belongs_to(Father)
            belongs_to(Company)
        class Post(ActiveRecord): 
            belongs_to(User)
        c = Collection(Post).where(name=['pengyi', 'poy']).joins({ 'user': [ {'father': 'company'}, 'company'] })
        sql, params = c.delete_or_update_or_find_sql()
        self.assertEqual('SELECT posts.* FROM posts INNER JOIN users ON users.id = posts.user_id INNER JOIN fathers ON fathers.id = users.father_id INNER JOIN companies ON companies.id = fathers.company_id INNER JOIN companies ON companies.id = users.company_id AND posts.name in (?, ?)', sql)
        self.assertEquals(['pengyi', 'poy'], params)

    # def test_sql_build_join_table_list_after__add_joins(self):
    #     class User(ActiveRecord): pass
    #     class Post(ActiveRecord): belongs_to(User)
    #     sb = Collection(Post)
    #     self.assertEqual([], sb._Collection__join_table_list)
    #     s = sb._Collection__add_joins('', ['user'])

    #     self.assertEqual(s, ' INNER JOIN users ON users.id = posts.user_id')
    #     self.assertEqual(['users'], sb._Collection__join_table_list)

    #     class Company(ActiveRecord): pass
    #     class Father(ActiveRecord): belongs_to(Company)
    #     class User(ActiveRecord): belongs_to(Father)
    #     class Post(ActiveRecord): 
    #         belongs_to(User)
    #         belongs_to(Father)
    #         belongs_to(Company)
    #     sb = Collection(Post)
    #     sb._Collection__add_joins('', [ 'company', {'father': 'company'}, {'user': {'father': 'company'}} ])
    #     self.assertEqual(['companies', 'fathers', 'companies', 'users', 'fathers', 'companies'], sb._Collection__join_table_list)

    #     self.assertEqual(3, len(sb._Collection__join_table_relation_dict()))
    #     self.assertTrue('companies' in sb._Collection__join_table_relation_dict)
    #     self.assertTrue('fathers' in sb._Collection__join_table_relation_dict)
    #     self.assertTrue('users' in sb._Collection__join_table_relation_dict)

    #     self.assertEqual(1, len(sb._Collection__join_table_relation_dict['companies']))
    #     self.assertTrue('companies' in sb._Collection__join_table_relation_dict['companies'])

    #     self.assertEqual(1, len(sb._Collection__join_table_relation_dict['users']))
    #     self.assertTrue('fathers' in sb._Collection__join_table_relation_dict['users'])
        
    #     self.assertEqual(1, len(sb._Collection__join_table_relation_dict['users']['fathers']))
    #     self.assertTrue('companies' in sb._Collection__join_table_relation_dict['users']['fathers'])


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
