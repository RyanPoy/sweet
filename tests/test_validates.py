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
from pyrails.activerecord import ActiveRecord
from pyrails.activerecord.validation import *
from pyrails.tests import create_table, drop_table
import unittest


class ValidatesTest(unittest.TestCase):
    
    def setUp(self):
        self.user_table_name = 'users'
        drop_table(self.user_table_name)
        create_table("""
create table if not exists %s (
    id int auto_increment,
    name varchar(32) not null,
    age int default 0,
    password varchar(32),
    gender varchar(4),
    PRIMARY KEY (id)
);""" % self.user_table_name)

    def tearDown(self):
        drop_table(self.user_table_name)

    def test_validates_percense_of(self):
        class User(ActiveRecord):
            validates_percense_of(['name', 'password'], msg=u'不能为空')

        user = User()
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(2, len(user.errors))
        self.assertEqual(u'不能为空', user.errors['name'][0])
        self.assertEqual(u'不能为空', user.errors['password'][0])

    def test_validates_length_of(self):
        class User(ActiveRecord):
            validates_length_of('name', _is=6, msg=u'长度必须是6')
        user = User()
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'长度必须是6', user.errors['name'][0])
        
    def test_validates_format_of(self):
        class User(ActiveRecord):
            validates_format_of('age', _with='^\d+$', msg=u'格式错误')
        user = User()
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'格式错误', user.errors['age'][0])

    def test_validates_inclusion_of(self):
        class User(ActiveRecord):
            validates_inclusion_of('gender', in_values=[u'男', u'女'], msg=u'只能是男和女')
        user = User()
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'只能是男和女', user.errors['gender'][0])

    def test_validates_exclusion_of(self):
        class User(ActiveRecord):
            validates_exclusion_of('gender', exclusion_values=[u'人妖', u'未知'], msg=u'不能是人妖和未知')
        user = User(gender=u'人妖')
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'不能是人妖和未知', user.errors['gender'][0])

    def test_validates_numericality_of(self):
        class User(ActiveRecord):
            validates_numericality_of('age', odd=True, msg=u'只能是奇数')
        user = User()
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'只能是奇数', user.errors['age'][0])

    def test_validates_confirmation_of(self):
        class User(ActiveRecord):
            validates_confirmation_of(['confirm_password'], msg=u'密码必须一致')
        user = User(password="123", confirm_password="456")
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'密码必须一致', user.errors['confirm_password'][0])

    def test_validates_uniqueness_of(self):
        class User(ActiveRecord):
            validates_uniqueness_of(['name'], msg=u'用户名已经存在')
        User.create(name='pengyi')
        user = User(name='pengyi')
        self.assertEqual(1, len(user.validate_func_dict))
        self.assertEqual(False, user.validate())
        self.assertEqual(1, len(user.errors))
        self.assertEqual(u'用户名已经存在', user.errors['name'][0])        


if __name__ == '__main__':
    unittest.main()
