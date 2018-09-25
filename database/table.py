#coding: utf8
from collections import namedtuple
from sweet.utils import *
import functools
import copy


def cp(method):
    @functools.wraps(method)
    def _(self, *args, **kwargs):
        tb = copy.deepcopy(self)
        return method(tb, *args, **kwargs)
    return _


class WhereExpr(object):

    ops = {
        'like': ('', 'like'),
        'not_like': ('', 'not like'),
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
        self.column = self.column.replace('__', '.')



class Table(object):

    LOCK = namedtuple("Lock", ['NILL', 'READ', 'WRITE'])._make([0, 1, 2])

    def __init__(self, db, tbname):
        self.db = db
        self.tbname = tbname
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
        self._lock = self.LOCK.NILL
        self._exists_tables = []

    def __deepcopy__(self, memo):
        """ Deep copy """
        obj = self.__class__(self.db, self.tbname)
        for k, v in self.__dict__.items():
            if k == 'db' or k == 'tbname': # conn can not deep copy
                obj.__dict__[k] = v
            else:
                obj.__dict__[k] = copy.deepcopy(v, memo)
        return obj

    @cp
    def distinct(self):
        self._distinct = True
        return self

    @cp
    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self._select.append(c)
        return self

    @property
    def bindings(self):
        return self._bindings

    @cp
    def where(self, **kwargs):
        return self.__where_or_having_or(self._wheres, self._where_bindings, 'AND', **kwargs)

    @cp
    def or_(self, **kwargs):
        return self.__where_or_having_or(self._wheres, self._where_bindings, 'OR', **kwargs)

    @cp
    def having(self, **kwargs):
        return self.__where_or_having_or(self._havings, self._having_bindings, 'AND', **kwargs)

    @cp
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

    @cp
    def group_by(self, *columns):
        for c in columns:
            self._group_bys.append(self.__aqm(c))
        return self

    @cp
    def order_by(self, column, desc=False):
        c = self.__aqm(column)
        if not desc:
            self._order_bys.append(c)
        else:
            self._order_bys.append('%s DESC' % c)
        return self

    @cp
    def limit(self, limit):
        self._limit = limit
        return self

    @cp
    def offset(self, offset):
        self._offset = offset
        return self

    @cp
    def page(self, page_num, page_size):
        page_num = 1 if page_num < 0 else page_num
        return self.limit((page_num-1) * page_size).offset(page_size)

    @cp
    def join(self, tbname, on):
        return self.__join('INNER JOIN', tbname, on)

    @cp
    def left_join(self, tbname, on):
        return self.__join('LEFT JOIN', tbname, on)

    @cp
    def right_join(self, tbname, on):
        return self.__join('RIGHT JOIN', tbname, on)

    @cp
    def read_lock(self):
        self._lock = self.LOCK.READ
        return self

    @cp
    def write_lock(self):
        self._lock = self.LOCK.WRITE
        return self

    @cp
    def where_exists(self, *tables):
        for t in tables:
            self._exists_tables.append( ("and", t) )
        return self

    @cp
    def or_exists(self, *tables):
        for t in tables:
            self._exists_tables.append( ("or", t) )
        return self

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
    _join_columns_sql = lambda self, columns: ', '.join(map(self.__aqm, columns))

    @property
    def sql(self):
        return 'SELECT {distinct}{columns} {from_sql}{lock_sql}'.format(
            distinct='DISTINCT ' if self._distinct else '', 
            columns=self._join_columns_sql(self._select) if self._select else '*',
            from_sql=self.__from_sql(),
            lock_sql=self.__lock_sql()
        )

    def __lock_sql(self):
        lock = ''
        if self._lock == self.LOCK.READ:
            lock = ' LOCK IN SHARE MODE'
        elif self._lock == self.LOCK.WRITE:
            lock = ' FOR UPDATE'
        return lock

    def __from_sql(self, for_query=True):
        sql = 'FROM {tablename}'.format(tablename=self.__aqm(self.tbname)) if for_query else ''
        join_sql = self.__join_sql
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        where_sql = self.__where_having_sql(self._wheres, self._where_bindings)
        if where_sql:
            sql = '%s WHERE %s' % (sql, where_sql)

        exists_tables_sql = self.__exists_tables_sql
        if exists_tables_sql:
            if not where_sql:
                if exists_tables_sql.startswith('AND '):
                    exists_tables_sql = exists_tables_sql[4:]
                elif exists_tables_sql.startswith('OR '):
                    exists_tables_sql = exists_tables_sql[3:]
                sql = '%s WHERE %s' % (sql, exists_tables_sql)
            else:
                sql = '%s %s' % (sql, exists_tables_sql)

        if self._group_bys:
            sql = '%s GROUP BY %s' % (sql, ', '.join(self._group_bys))

        having_sql = self.__where_having_sql(self._havings, self._having_bindings)
        if having_sql:
            sql = '%s HAVING %s' % (sql, having_sql)

        if self._order_bys:
            sql = '%s ORDER BY %s' % (sql, ', '.join(self._order_bys))

        limit_and_offset_sql = self.__limit_and_offset_sql()
        if limit_and_offset_sql:
            sql = '%s %s' % (sql, limit_and_offset_sql)

        return sql

    @property
    def __exists_tables_sql(self):
        sql = []
        for or_and, t in self._exists_tables:
            if or_and == 'and':
                sql.append('AND EXISTS (%s)' % t.sql)
                self.bindings.extend(t.bindings)
            else:
                sql.append('OR EXISTS (%s)' % t.sql)
                self.bindings.extend(t.bindings)
        return ' '.join(sql)

    @property
    def __join_sql(self):
        sqls = []
        for j in self._joins:
            on = ' = '.join([ self.__aqm(x.strip()) for x in j.on.split('=') ])
            sqls.append('%s %s ON %s' % (j.join_type, self.__aqm(j.tbname), on))
        return ' '.join(sqls)

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
                    sqls.append('%s %s %s %s AND %s' % (and_or, k, bt, self.paramstyle_marks, self.paramstyle_marks))
                    self._bindings.extend(v)
                else:
                    if is_array(v):
                        _in = 'NOT IN' if is_or_not == 'not' else 'IN'
                        sqls.append('%s %s %s (%s)' % (and_or, k, _in, ', '.join([self.paramstyle_marks]*len(v))) )
                        self._bindings.extend(v)
                    elif v is None:
                        _is = 'IS NOT NULL' if is_or_not == 'not' else 'IS NULL'
                        sqls.append('%s %s %s' % (and_or, k, _is) )
                    else:
                        sqls.append('%s %s %s %s' % (and_or, k, op, self.paramstyle_marks) )
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

    def insert_getid(self, record=None, **kwargs):
        record = record or {}
        if kwargs:
            record.update(kwargs)
        
        sql = 'INSERT INTO {tablename} ({columns}) VALUES {values_sql}'.format(
            tablename=self.__aqm(self.tbname),
            columns=self._join_columns_sql(record.keys()),
            values_sql='(%s)' % ', '.join([self.paramstyle_marks]*len(record))
        )    
        return self.db.execute_lastrowid(sql, *record.values())

    def insert(self, records=None, **kwargs):
        list_records = []
        if records:
            if is_hash(records):
                list_records.append(records)
            elif is_array(records):
                list_records.extend(records)

        if kwargs:
            list_records.append(kwargs)

        if not list_records:
            return 0 # nothing insert

        if len(list_records) > 1:
            keys = list_records[0].keys()
            for r in list_records:
                if r.keys() != keys:
                    raise Exception("multiple insert only support same keys records")

        values_sql, bindings = [], []
        for r in list_records:
            values_sql.append('(%s)' % ', '.join([self.paramstyle_marks]*len(r)))
            bindings.extend(r.values())
        
        sql = 'INSERT INTO {tablename} ({columns}) VALUES {values_sql}'.format(
            tablename=self.__aqm(self.tbname),
            columns=self._join_columns_sql(list_records[0].keys()),
            values_sql=', '.join(values_sql)
        )    
        return self.db.execute_rowcount(sql, *bindings)

    def update(self, **kwargs):
        columns, new_bindings = [], []
        for k, v in kwargs.items():
            columns.append('%s = %%s' % self.__aqm(k))
            new_bindings.append(v)

        sql = 'UPDATE {tbname} SET {columns}{from_sql}'.format(
            tbname=self.__aqm(self.tbname),
            columns=', '.join(columns),
            from_sql=self.__from_sql(False)
        )
        new_bindings.extend(self.bindings)
        return self.db.execute_rowcount(sql, *new_bindings)

    def increase(self, **kwargs):
        return self.__in_or_decrease('in', **kwargs)

    def decrease(self, **kwargs):
        return self.__in_or_decrease('de', **kwargs)

    def __in_or_decrease(self, in_or_de='in', **kwargs):
        flag = '+' if in_or_de == 'in' else '-'
        columns, new_bindings = [], []
        for k, v in kwargs.items():
            columns.append('{name} = {name} {flag} %s'.format(name=self.__aqm(k), flag=flag))
            new_bindings.append(v)

        sql = 'UPDATE {tbname} SET {columns}{from_sql}'.format(
            tbname=self.__aqm(self.tbname),
            columns=', '.join(columns),
            from_sql=self.__from_sql(False)
        )
        new_bindings.extend(self.bindings)
        return self.db.execute_rowcount(sql, *new_bindings)        

    def delete(self):
        if not self._joins: # needn't join
            sql = "DELETE {from_sql}".format(from_sql=self.__from_sql())
        else:
            sql = "DELETE {tablename} {from_sql}".format(
                tablename=self.__aqm(self.tbname),
                from_sql=self.__from_sql()
            )
        return self.db.execute_rowcount(sql, *self.bindings)

    def truncate(self):
        return self.db.execute_rowcount('TRUNCATE {}'.format(self.__aqm(self.tbname)))

    def first(self):
        return self.db.fetchone(self.sql, *self.bindings)

    def last(self):
        return self.db.fetchlastone(self.sql, *self.bindings)

    def all(self):
        return self.db.fetchall(self.sql, *self.bindings)

    def exists(self):
        return True if self.first() else False

    def count(self, column=None, distinct=False):
        return self.__func('count', column, distinct)

    def max(self, column, distinct=False):
        return self.__func('max', column, distinct)

    def min(self, column, distinct=False):
        return self.__func('min', column, distinct)

    def avg(self, column, distinct=False):
        return self.__func('average', column, distinct)

    def sum(self, column, distinct=False):
        return self.__func('sum', column, distinct)

    def __func(self, func_name, column, distinct=False):
        func_name = func_name.upper()

        if not column:
            column = '*'
        if column == '*':
            distinct = False

        column_name = self.__aqm(column) if column else '*'
        sql = 'SELECT {func_name}({column_name}) AS aggregate {from_sql}'.format(
            func_name=func_name,
            column_name=column_name if not distinct else 'DISTINCT %s' % column_name,
            from_sql=self.__from_sql()
        )
        vs = self.db.fetchone(sql, *self.bindings)
        return vs.aggregate


class MySQLTable(Table):

    qutotation_marks = '`'
    paramstyle_marks = '%s'

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
    