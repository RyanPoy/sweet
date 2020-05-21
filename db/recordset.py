#coding: utf8
from collections import namedtuple
from sweet.db.clauses import *
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

    def __init__(self, db, tbname, model_class=None):
        self.db = db
        self.tbname = tbname
        self.select_clause = SelectClause(self.qutotation_marks)
        self.where_clause = WhereClause(self.qutotation_marks, self.paramstyle_marks)
        self.having_clause = HavingClause(self.qutotation_marks, self.paramstyle_marks)
        self.group_clause = GroupClause(self.qutotation_marks)
        self.order_clause = OrderClause(self.qutotation_marks)
        self._joins_clauses = []
        self.page_clause = PageClause()
        self._lock = self.LOCK.NILL
        self._exists_tables = []
        self.unions = []
        self.model_class = model_class
        self._includes = []

    def __deepcopy__(self, memo):
        """ Deep copy """
        obj = self.__class__(self.db, self.tbname)
        for k, v in self.__dict__.items():
            if k == 'db' or k == 'tbname': # conn can not deep copy
                obj.__dict__[k] = v
            else:
                obj.__dict__[k] = copy.deepcopy(v, memo)
        return obj

    def union(self, rs):
        self.unions.append( (rs, False) )
        return self

    def union_all(self, rs):
        self.unions.append( (rs, True) )
        return self

    @dcp
    def distinct(self):
        self.select_clause.distinct()
        return self

    @dcp
    def select(self, *columns):
        self.select_clause.select(*columns)
        return self

    @dcp
    def where(self, *where_clauses, **kwargs):
        self.where_clause.and_(*where_clauses, **kwargs)
        return self

    @dcp
    def or_where(self, *where_clauses, **kwargs):
        self.where_clause.or_(*where_clauses, **kwargs)
        return self

    @dcp
    def having(self, *where_clauses, **kwargs):
        self.having_clause.and_(*where_clauses, **kwargs)
        return self

    @dcp
    def or_having(self, *where_clauses, **kwargs):
        self.having_clause.or_(*where_clauses, **kwargs)
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

    def join(self, tbname, on=None, func=None):
        return self.__join(JoinClause, tbname, on, func)

    def left_join(self, tbname, on=None, func=None):
        return self.__join(LeftJoinClause, tbname, on, func)

    def cross_join(self, tbname, on=None, func=None):
        return self.__join(CrossJoinClause, tbname, on, func)

    def right_join(self, tbname, on=None, func=None):
        return self.__join(RightJoinClause, tbname, on, func)

    @dcp
    def __join(self, join_clause_class, tbname, on, func=None):
        jc = join_clause_class(self.qutotation_marks, self.paramstyle_marks, tbname)
        if on:
            jc.on(on)

        if func: 
            func(jc)
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
    def where_exists(self, *rset):
        for t in rset:
            self._exists_tables.append( (WhereClause.AND, t) )
        return self

    @dcp
    def or_exists(self, *rset):
        for t in rset:
            self._exists_tables.append( (WhereClause.OR, t) )
        return self

    def __aqm(self, s):
        return aqm(s, self.qutotation_marks)

    def _query_sql(self):
        params = []
        from_sql = self.__from_sql(params)
        select_sql, select_params = self.select_clause.compile()
        sql = '{select_sql} {from_sql}{lock_sql}'.format(
            select_sql= select_sql,
            from_sql=from_sql,
            lock_sql=self.__lock_sql()
        )
        params.extend(select_params)
        return sql, params

    @property
    def tablename(self):
        return self.__aqm(self.tbname)

    def __lock_sql(self):
        lock = ''
        if self._lock == self.LOCK.READ:
            lock = ' LOCK IN SHARE MODE'
        elif self._lock == self.LOCK.WRITE:
            lock = ' FOR UPDATE'
        return lock

    def __push_exist_sql(self, where_sql, sql, params):
        sqls = []
        for or_and, t in self._exists_tables:
            tmp_sql, tmp_params = t._query_sql()
            sqls.append('%s EXISTS (%s)' % (or_and, tmp_sql))
            params.extend(tmp_params)
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

    def __from_sql(self, params):
        sql = 'FROM {tablename}'.format(tablename=self.tablename)
        join_sql = self.__join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)
        return self.__core_sql(sql, params)

    def __core_sql(self, sql, params):
        where_sql, where_params = self.where_clause.compile()
        if where_sql:
            sql = '%s %s' % (sql, where_sql)
            params.extend(where_params)

        sql = self.__push_exist_sql(where_sql, sql, params)
        group_sql, group_params = self.group_clause.compile()
        if group_sql:
            sql = '%s %s' % (sql, group_sql)
            params.extend(group_params)

        having_sql, having_params = self.having_clause.compile()
        if having_sql:
            sql = '%s %s' % (sql, having_sql)
            params.extend(having_params)

        union_sql = self.__union_sql(params)
        if union_sql:
            sql = '%s %s' % (sql, union_sql)

        order_sql, order_params = self.order_clause.compile()
        if order_sql:
            sql = '%s %s' % (sql, order_sql)
            params.extend(order_params)

        limit_and_offset_sql, limit_and_offset_params = self.page_clause.compile()
        if limit_and_offset_sql:
            sql = '%s %s' % (sql, limit_and_offset_sql)
            params.extend(limit_and_offset_params)

        return sql

    def __union_sql(self, params):
        sqls = []
        for u, _all in self.unions:
            sqls.append( 'UNION ALL' if _all else 'UNION' )
            tmp_sql, tmp_params = u._query_sql()
            sqls.append(tmp_sql)
            params.extend(tmp_params)
        return ' '.join(sqls)

    def __join_sql(self, params):
        sqls = []
        for j in self._joins_clauses:
            tmp_sql, tmp_params = j.compile()
            if tmp_sql:
                sqls.append(tmp_sql)
            params.extend(tmp_params)
        return ' '.join(sqls)

    def insert_getid(self, record=None, **kwargs):
        record = record or {}
        if kwargs: record.update(kwargs)
        insert_clause = InsertClause(self.qutotation_marks, self.paramstyle_marks, self.tbname)
        sql, params = insert_clause.insert(record).compile()
        return self.db.execute_lastrowid(sql, *params)

    def insert(self, records=None, **kwargs):
        insert_clause = InsertClause(self.qutotation_marks, self.paramstyle_marks, self.tbname)
        sql, params = insert_clause.insert(records, **kwargs).compile()
        return self.db.execute_rowcount(sql, *params)

    @dcp
    def update(self, **kwargs):
        update_columns, update_params = [], []
        for k, v in kwargs.items():
            update_columns.append('%s = %%s' % self.__aqm(k))
            update_params.append(v)

        sql = 'UPDATE %s' % self.tablename
        params = [] 
        join_sql = self.__join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        params.extend(update_params)

        sql = self.__core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    def increase(self, **kwargs):
        return self.__in_or_decrease('+', **kwargs)

    def decrease(self, **kwargs):
        return self.__in_or_decrease('-', **kwargs)

    @dcp
    def __in_or_decrease(self, flag='+', **kwargs):
        update_columns, update_params = [], []
        for k, v in kwargs.items():
            update_columns.append('{name} = {name} {flag} %s'.format(name=self.__aqm(k), flag=flag))
            update_params.append(v)

        params = []
        sql = 'UPDATE %s' % self.tablename 
        join_sql = self.__join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        params.extend(update_params)

        sql = self.__core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    def delete(self):
        if self.model_class:
            # mean that: User.where(age__gt=30).delete()
            self.model_class._delete_relations(self.all())

        params = []
        from_sql = self.__from_sql(params)
        if not self._joins_clauses: # needn't join
            sql = "DELETE {from_sql}".format(from_sql=from_sql)
        else:
            sql = "DELETE {tablename} {from_sql}".format(
                tablename=self.tablename,
                from_sql=from_sql
            )
        return self.db.execute_rowcount(sql, *params)

    def truncate(self):
        return self.db.execute_rowcount('TRUNCATE {}'.format(self.tablename))

    def first(self):
        sql, params = self._query_sql()
        r = self.db.fetchone(sql, *params)
        if not r or not self.model_class:
            return r
        m = self.model_class(**r)
        self.model_class._get_include([m], self._includes)
        return m

    def last(self):
        sql, params = self._query_sql()
        r = self.db.fetchlastone(sql, *params)
        if not r or not self.model_class:
            return r
        m = self.model_class(**r)
        self.model_class._get_include([m], self._includes)
        return m

    def all(self):
        sql, params = self._query_sql()
        rs = self.db.fetchall(sql, *params)
        if not rs or not self.model_class:
            return rs
        ms = [ self.model_class(**r) for r in rs ]
        self.model_class._get_include(ms, self._includes)
        return ms

    def exists(self):
        return True if self.first() else False

    def count(self, column=None, distinct=False):
        return self.__func('count', column, distinct)

    def max(self, column, distinct=False):
        return self.__func('max', column, distinct)

    def min(self, column, distinct=False):
        return self.__func('min', column, distinct)

    def avg(self, column, distinct=False):
        return self.__func('avg', column, distinct)

    def sum(self, column, distinct=False):
        return self.__func('sum', column, distinct)

    def __func(self, func_name, column, distinct=False):
        func_name = func_name.upper()

        if not column:
            column = '*'

        if column == '*':
            distinct = False

        column_name = self.__aqm(column)

        params = []
        from_sql = self.__from_sql(params)
        sql = 'SELECT {func_name}({column_name}) AS aggregate {from_sql}'.format(
            func_name=func_name,
            column_name=column_name if not distinct else 'DISTINCT %s' % column_name,
            from_sql=from_sql
        )
        vs = self.db.fetchone(sql, *params)
        return vs.aggregate


class MySQLRecordset(Recordset):

    qutotation_marks = '`'
    paramstyle_marks = '%s'
