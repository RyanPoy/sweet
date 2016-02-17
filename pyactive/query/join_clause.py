#coding: utf8
from ..utils import is_hash, is_array

class JoinClause(object):
    
    def __init__(self, tablename, direction='INNER'):
        self.tablename = tablename
        self.direction = direction
        self.ons = []
        
    def on(self, *args, **kwargs):
        if args:
            self.ons.append(args)
        if kwargs:
            self.ons.append(kwargs)
        return self

    def to_sql(self):
        sqls, params = ['%s JOIN `%s`' % (self.direction, self.tablename)], []
        on_sql, on_params = self._compile_on()
        if on_sql:
            sqls.append('ON %s' % on_sql)
            params += on_params
        return ' '.join(sqls), params

    def _compile_on(self):
        sqls, params = [], []
        for condition in self.ons:
            if is_array(condition):
                if len(condition) == 1: # 只有一个，表示是一个不带参数的sql
                    sqls.append(condition[0])
                else:
                    sqls.append(condition[0])
                    params.extend(condition[1:])
            elif is_hash(condition): # 表示是 key:value 的方式, eg. where(name='abc').where(age=1)
                for k, v in condition.iteritems():
                    if is_array(v):
                        if v: # 数组里面有元素
                            sqls.append('%s IN (%s)' % (self._compile_fieldname(k), self._postion_flag(v)))
                            params.extend(list(v)) # 如果是set，就必须转一下
                    elif v is None:
                        sqls.append('%s IS NULL' % self._compile_fieldname(k))
                    else:
                        sqls.append('%s = %s' % (self._compile_fieldname(k), self.__class__.POSITION_FLAG))
                        params.append(v)
        sql = ' AND '.join(sqls)
        return sql, params
