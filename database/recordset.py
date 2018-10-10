#coding: utf8
from collections import namedtuple
from sweet.database.clauses import *
from sweet.utils import *
import functools
import copy


def dcp(method):
    @functools.wraps(method)
    def _(self, *args, **kwargs):
        tb = copy.deepcopy(self)
        return method(tb, *args, **kwargs)
    return _


class Recordset(object):

    LOCK = namedtuple("Lock", ['NILL', 'READ', 'WRITE'])._make([0, 1, 2])

    def __init__(self, db, tbname):
        self.db = db
        self.tbname = tbname
        self.select_clause = SelectClause(self.qutotation_marks)
        self.where_clause = WhereClause(self.qutotation_marks, self.paramstyle_marks)
        self.having_clause = HavingClause(self.qutotation_marks, self.paramstyle_marks)
        self._bindings = []
        self.group_clause = GroupClause(self.qutotation_marks)
        self.order_clause = OrderClause(self.qutotation_marks)
        self._joins_clauses = []
        self.page_clause = PageClause()
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

    @dcp
    def distinct(self):
        self.select_clause.distinct()
        return self

    @dcp
    def select(self, *columns):
        self.select_clause.select(*columns)
        return self

    @property
    def bindings(self):
        return self._bindings

    @dcp
    def where(self, **kwargs):
        self.where_clause.and_(**kwargs)
        return self

    @dcp
    def or_(self, **kwargs):
        self.where_clause.or_(**kwargs)
        return self

    @dcp
    def having(self, **kwargs):
        self.having_clause.and_(**kwargs)
        return self

    @dcp
    def or_having(self, **kwargs):
        self.having_clause.or_(**kwargs)
        return self

    @dcp
    def group_by(self, *columns):
        self.group_clause.by(*columns)
        return self

    @dcp
    def order_by(self, column, desc=False):
        self.order_clause.by(column, desc)
        return self

    @dcp
    def limit(self, num):
        self.page_clause.limit(num)
        return self

    @dcp
    def offset(self, num):
        self.page_clause.offset(num)
        return self

    @dcp
    def page(self, page_num, page_size):
        page_num = 1 if page_num < 0 else page_num
        return self.limit((page_num-1) * page_size).offset(page_size)

    def join(self, tbname, on, func=None):
        return self.__join(JoinClause, tbname, on, func)

    def left_join(self, tbname, on, func=None):
        return self.__join(LeftJoinClause, tbname, on, func)

    def right_join(self, tbname, on, func=None):
        return self.__join(RightJoinClause, tbname, on, func)

    @dcp
    def __join(self, join_clause_clazz, tbname, on, func=None):
        jc = join_clause_clazz(self.qutotation_marks, self.paramstyle_marks, tbname)
        if on:
            jc.on(on)
        if func: 
            jc = func(jc)
        self._joins_clauses.append(jc)
        return self

    @dcp
    def read_lock(self):
        self._lock = self.LOCK.READ
        return self

    @dcp
    def write_lock(self):
        self._lock = self.LOCK.WRITE
        return self

    @dcp
    def where_exists(self, *tables):
        for t in tables:
            self._exists_tables.append( (WhereClause.AND, t) )
        return self

    @dcp
    def or_exists(self, *tables):
        for t in tables:
            self._exists_tables.append( (WhereClause.OR, t) )
        return self

    def __aqm(self, s):
        return aqm(s, self.qutotation_marks)
    _join_columns_sql = lambda self, columns: ', '.join([ aqm(c, self.qutotation_marks) for c in columns ])

    @property
    def sql(self):
        return '{select_sql} {from_sql}{lock_sql}'.format(
            select_sql=self.select_clause.compile().sql,
            from_sql=self.__from_sql,
            lock_sql=self.__lock_sql()
        )

    def __lock_sql(self):
        lock = ''
        if self._lock == self.LOCK.READ:
            lock = ' LOCK IN SHARE MODE'
        elif self._lock == self.LOCK.WRITE:
            lock = ' FOR UPDATE'
        return lock

    def __push_exist_sql(self, where_sql, sql):
        sqls = []
        for or_and, t in self._exists_tables:
            sqls.append('%s EXISTS (%s)' % (or_and, t.sql))
            self.bindings.extend(t.bindings)
        exists_tables_sql = ' '.join(sqls)

        if exists_tables_sql:
            if not where_sql:
                if exists_tables_sql.startswith('AND '):
                    exists_tables_sql = exists_tables_sql[4:]
                elif exists_tables_sql.startswith('OR '):
                    exists_tables_sql = exists_tables_sql[3:]
                sql = '%s WHERE %s' % (sql, exists_tables_sql)
            else:
                sql = '%s %s' % (sql, exists_tables_sql)
        return sql

    @property
    def __from_sql(self):
        sql = 'FROM {tablename}'.format(tablename=self.__aqm(self.tbname))
        join_sql = self.__join_sql
        if join_sql:
            sql = '%s %s' % (sql, join_sql)
        return self.__core_sql(sql)

    def __core_sql(self, sql):
        where_sql = self.where_clause.compile().sql
        if where_sql:
            sql = '%s %s' % (sql, where_sql)
            self.bindings.extend(self.where_clause.bindings)

        sql = self.__push_exist_sql(where_sql, sql)
        group_sql = self.group_clause.compile().sql
        if group_sql:
            sql = '%s %s' % (sql, group_sql)

        having_sql = self.having_clause.compile().sql
        if having_sql:
            sql = '%s %s' % (sql, having_sql)
            self.bindings.extend(self.having_clause.bindings)

        order_sql = self.order_clause.compile().sql
        if order_sql:
            sql = '%s %s' % (sql, order_sql)

        limit_and_offset_sql = self.page_clause.compile().sql
        if limit_and_offset_sql:
            sql = '%s %s' % (sql, limit_and_offset_sql)

        return sql

    @property
    def __join_sql(self):
        sqls = []
        for j in self._joins_clauses:
            j.compile()
            if j.sql:
                sqls.append(j.sql)
            self.bindings.extend(j.bindings)
        return ' '.join(sqls)

    def insert_getid(self, record=None, **kwargs):
        record = record or {}
        if kwargs: record.update(kwargs)
        insert_clause = InsertClause(self.qutotation_marks, self.paramstyle_marks, self.tbname)
        insert_clause.insert(record).compile()
        return self.db.execute_lastrowid(insert_clause.sql, *insert_clause.bindings)

    def insert(self, records=None, **kwargs):
        insert_clause = InsertClause(self.qutotation_marks, self.paramstyle_marks, self.tbname)
        insert_clause.insert(records, **kwargs).compile()
        return self.db.execute_rowcount(insert_clause.sql, *insert_clause.bindings)

    @dcp
    def update(self, **kwargs):
        update_columns, update_bindings = [], []
        for k, v in kwargs.items():
            update_columns.append('%s = %%s' % self.__aqm(k))
            update_bindings.append(v)

        sql = 'UPDATE %s' % self.__aqm(self.tbname) 
        join_sql = self.__join_sql
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        self.bindings.extend(update_bindings)

        sql = self.__core_sql(sql)

        return self.db.execute_rowcount(sql, *self.bindings)

    def increase(self, **kwargs):
        return self.__in_or_decrease('+', **kwargs)

    def decrease(self, **kwargs):
        return self.__in_or_decrease('-', **kwargs)

    @dcp
    def __in_or_decrease(self, flag='+', **kwargs):
        update_columns, update_bindings = [], []
        for k, v in kwargs.items():
            update_columns.append('{name} = {name} {flag} %s'.format(name=self.__aqm(k), flag=flag))
            update_bindings.append(v)

        sql = 'UPDATE %s' % self.__aqm(self.tbname) 
        join_sql = self.__join_sql
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        self.bindings.extend(update_bindings)

        sql = self.__core_sql(sql)

        return self.db.execute_rowcount(sql, *self.bindings)        

    def delete(self):
        if not self._joins_clauses: # needn't join
            sql = "DELETE {from_sql}".format(from_sql=self.__from_sql)
        else:
            sql = "DELETE {tablename} {from_sql}".format(
                tablename=self.__aqm(self.tbname),
                from_sql=self.__from_sql
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

        column_name = self.__aqm(column)
        sql = 'SELECT {func_name}({column_name}) AS aggregate {from_sql}'.format(
            func_name=func_name,
            column_name=column_name if not distinct else 'DISTINCT %s' % column_name,
            from_sql=self.__from_sql
        )
        vs = self.db.fetchone(sql, *self.bindings)
        return vs.aggregate


class MySQLRecordset(Recordset):

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
    