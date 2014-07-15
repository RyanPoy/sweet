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
from pyrails.activesupport import RecordNotFound, is_array, is_str, is_hash, flatten, to_i


class WhereChain(object):

    def __init__(self, table_name):
        self.table_name = table_name
        self.conditions = []

    def push(self, *args, **kwargs):
        if args:
            self.conditions.append(args)
        elif kwargs:
            self.conditions.append(kwargs)
        return self

    def compile(self, prefix='WHERE'):
        sql, params = self._compile()
        if sql:
            sql = '%s %s' % ( prefix, sql )
        return sql, params

    def _compile(self):
        sqls, params = [], []
        for condition in self.conditions:
            if is_array(condition):
                if len(condition) == 1: # 只有一个，表示是一个不带参数的sql
                    sqls.append(condition[0])
                else:
                    sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition): # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if is_array(v):
                        if len(v) == 1:
                            sqls.append('`%s`.`%s` = ?' % (self.table_name, k))
                            params.extend(list(v)) # 如果是set，就必须转一下
                        else:
                            interrogations = ', '.join(['?'] * len(v))
                            sqls.append('`%s`.`%s` in (%s)' % (self.table_name, k, interrogations))
                            params.extend(list(v)) # 如果是set，就必须转一下
                    else:
                        sqls.append('`%s`.`%s` = ?' % (self.table_name, k))
                        params.append(v)
        return ' AND '.join(sqls), params


class HavingChain(WhereChain):

    def compile(self):
        sql, params = self._compile()
        if sql:
            sql = 'HAVING %s' % sql
        return sql, params

