#coding: utf8
from sweet.utils import *


class WhereExpr(object):

    ops = {
        'gt': ('', '>'), 
        'not_gt': ('not', '>'), 
        'gte': ('', '>='), 
        'not_gte': ('not', '>='), 
        'lt': ('', '<'), 
        'not_lt': ('not', '<'), 
        'lte': ('', '<='), 
        'not_lte': ('not', '<='),
        'not': ('not', '!='),
        'bt': ('', 'between'), # between
        'not_bt': ('not', 'between') # not between
    }

    def __init__(self, s):
        vs = s.split('__')
        if vs[-1] in self.ops: # 后缀在 ops里面
            self.is_or_not, self.operator = self.ops[vs[-1]]
            self.column = s[:-len(vs[-1])-2]
        else:
            self.is_or_not, self.column, self.operator = '', s, '='



class Table(object):

    def __init__(self):
        self.tbname = ''
        self._select = []
        self._distinct = False
        self._wheres = []
        self._or = []
        self._where_bindings = []
        self._havings = []
        self._or_having = []
        self._having_bindings = []
        self._bindings = []
        self._group_bys = []
        self._order_bys = []
        self._joins = []
        self._limit = None
        self._offset = None

    def distinct(self):
        self._distinct = True

    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self._select.append(c)
        return self

    @property
    def bindings(self):
        return self._bindings

    def from_(self, tbname):
        self.tbname = tbname
        return self

    def where(self, **kwargs):
        return self.__where_or_having_or(self._wheres, self._where_bindings, 'AND', **kwargs)

    def or_(self, **kwargs):
        return self.__where_or_having_or(self._wheres, self._where_bindings, 'OR', **kwargs)

    def having(self, **kwargs):
        return self.__where_or_having_or(self._havings, self._having_bindings, 'AND', **kwargs)

    def or_having(self, **kwargs):
        return self.__where_or_having_or(self._havings, self._having_bindings, 'OR', **kwargs)

    def __where_or_having_or(self, wheres_or_havings, where_or_having_bindings, and_or, **kwargs):
        for k, v in kwargs.items():
            expr = WhereExpr(k)
            wheres_or_havings.append(mydict(
                and_or = and_or,
                is_or_not = expr.is_or_not,
                column = expr.column,
                operator = expr.operator
            ))
            where_or_having_bindings.append(v)

        return self

    def group_by(self, *columns):
        for c in columns:
            self._group_bys.append(self.__aqm(c))
        return self

    def order_by(self, column, desc=False):
        c = self.__aqm(column)
        if not desc:
            self._order_bys.append(c)
        else:
            self._order_bys.append('%s DESC' % c)
        return self

    def limit(self, limit):
        self._limit = limit
        return self

    def offset(self, offset):
        self._offset = offset
        return self

    def page(self, page_num, page_size):
        page_num = 1 if page_num < 0 else page_num
        self.limit((page_num-1) * page_size).offset(page_size)
        return self

    def join(self, tbname, on):
        return self.__join('INNER JOIN', tbname, on)

    def left_join(self, tbname, on):
        return self.__join('LEFT JOIN', tbname, on)

    def right_join(self, tbname, on):
        return self.__join('RIGHT JOIN', tbname, on)

    def __join(self, join_type, tbname, on):
        self._joins.append(mydict(
            join_type = join_type,
            tbname = tbname,
            on = on
        ))
        return self

    def __add_quotation_marks(self, s):
        if s == '*':
            return s
        return '.'.join([ '%s%s%s' % (self.qutotation_marks, x, self.qutotation_marks) for x in s.split('.') ])

    __aqm = __add_quotation_marks

    @property
    def sql(self):
        sql = 'SELECT {columns} FROM {tablename}'.format(
            columns = ', '.join(map(self.__aqm, self._select)) if self._select else '*',
            tablename = self.__aqm(self.tbname)
        )
        join_sql = self.join_sql
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        where_sql = self.where_sql
        if where_sql:
            if join_sql:
                sql = '%s AND %s' % (sql, where_sql)
            else:
                sql = '%s WHERE %s' % (sql, where_sql)

        if self._group_bys:
            sql = '%s GROUP BY %s' % (sql, ', '.join(self._group_bys))

        having_sql = self.having_sql
        if having_sql:
            sql = '%s HAVING %s' % (sql, having_sql)

        if self._order_bys:
            sql = '%s ORDER BY %s' % (sql, ', '.join(self._order_bys))

        limit_and_offset_sql = self.__limit_and_offset_sql()
        if limit_and_offset_sql:
            sql = '%s %s' % (sql, limit_and_offset_sql)

        return sql

    @property
    def join_sql(self):
        sqls = []
        for j in self._joins:
            on = ' = '.join([ self.__aqm(x.strip()) for x in j.on.split('=') ])
            sqls.append('%s %s ON %s' % (j.join_type, self.__aqm(j.tbname), on))
        return ' '.join(sqls)

    @property
    def where_sql(self):
        return self.__where_having_sql(self._wheres, self._where_bindings)

    @property
    def having_sql(self):
        return self.__where_having_sql(self._havings, self._having_bindings)

    def __where_having_sql(self, wheres_or_havings, where_or_having_bindings):
        sqls = []
        if wheres_or_havings:
            for i, w in enumerate(wheres_or_havings):
                k, op = self.__aqm(w.column), w.operator
                is_or_not, and_or = w.is_or_not, w.and_or

                v = where_or_having_bindings[i]
                if op == 'between':
                    # between 要做特殊处理
                    if not is_array(v) or len(v) != 2:
                        raise TypeError('Should give between 2 params, but %s' % v)
                    bt = 'NOT BETWEEN' if is_or_not == 'not' else 'BETWEEN'
                    sqls.append('%s %s %s %%s AND %%s' % (and_or, k, bt))
                    self._bindings.extend(v)
                else:
                    if is_array(v):
                        _in = 'NOT IN' if is_or_not == 'not' else 'IN'
                        sqls.append('%s %s %s (%s)' % (and_or, k, _in, ', '.join(['%s']*len(v))) )
                        self._bindings.extend(v)
                    elif v is None:
                        _is = 'IS NOT NULL' if is_or_not == 'not' else 'IS NULL'
                        sqls.append('%s %s %s' % (and_or, k, _is) )
                    else:
                        sqls.append('%s %s %s %%s' % (and_or, k, op) )
                        self._bindings.append(v)
        
        if sqls:
            s = ' '.join(sqls)
            if s.startswith('AND '):
                s = s[4:]
            elif s.startswith('OR '):
                s = s[3:]
            return s
        return ''

    def __limit_and_offset_sql(self):
        sqls = []
        if self._limit is not None:
            sqls.append('LIMIT %s' % self._limit)
        if self._offset:
            sqls.append('OFFSET %s' % self._offset)
        return ' '.join(sqls)


class MySQLTable(Table):

    qutotation_marks = '`'

    def list_column_sql(self, dbname, tbname):
        return '''
SELECT 
    COLUMN_NAME AS field, 
    COLUMN_TYPE AS type, 
    IS_NULLABLE AS `null`, 
    COLUMN_KEY AS `key`, 
    COLUMN_DEFAULT AS `default`, 
    EXTRA AS extra, 
    COLUMN_COMMENT AS comment, 
    CHARACTER_SET_NAME AS character_set, 
    COLLATION_NAME AS collation
FROM 
    information_schema.COLUMNS 
WHERE 
    TABLE_SCHEMA = "%s" 
AND 
    TABLE_NAME = "%s"''' % (dbname, tbname)
    