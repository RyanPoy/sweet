    #coding: utf8
from collections import namedtuple
from sweet.record.clauses import *
from sweet.utils import *
from abc import abstractmethod, ABC
import functools
import copy


class SQLError(Exception): pass

def dcp(method):
    @functools.wraps(method)
    def _(self, *args, **kwargs):
        tb = copy.deepcopy(self)
        return method(tb, *args, **kwargs)
    return _


class Recordset(ABC):

    LOCK = namedtuple("Lock", ['NILL', 'READ', 'WRITE'])._make([0, 1, 2])

    def __init__(self, db, tablename, model_class=None, pk='id'):
        """
        :param pk: primary key of the table. it will be used on delete with join、update with join
        pk = model_class.__pk__ if model_class is not None
        """
        self.db = db
        self.tablename = tablename
        self.aqm_tablename = self.db.aqm(tablename)
        self.select_clause = SelectClause()
        self.where_clause = WhereClause()
        self.having_clause = HavingClause()
        self.group_clause = GroupClause()
        self.order_clause = OrderClause()
        self._joins_clauses = []
        self.page_clause = PageClause()
        self._lock = self.LOCK.NILL
        self._exists_tables = []
        self.unions = []
        self._includes = []
        self.table_pk = pk
        self.model_class = None
        self.set_model_class(model_class)

    def __deepcopy__(self, memo):
        """ Deep copy """
        obj = self.__class__(self.db, self.aqm_tablename)
        for k, v in self.__dict__.items():
            if k == 'database' or k == 'tablename': # conn can not deep copy
                obj.__dict__[k] = v
            else:
                obj.__dict__[k] = copy.deepcopy(v, memo)
        return obj

    def set_model_class(self, model_class):
        self.model_class = model_class
        if self.model_class:
            self.table_pk = self.model_class.__pk__
        return self

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
        return self._join(JoinClause, tbname, on, func)

    def left_join(self, tbname, on=None, func=None):
        return self._join(LeftJoinClause, tbname, on, func)

    def cross_join(self, tbname, on=None, func=None):
        return self._join(CrossJoinClause, tbname, on, func)

    def right_join(self, tbname, on=None, func=None):
        return self._join(RightJoinClause, tbname, on, func)

    @dcp
    def _join(self, join_clause_class, tbname, on, func=None):
        jc = join_clause_class(tbname)
        if on:
            jc.on(on)

        if func: 
            func(jc)
        self._joins_clauses.append(jc)
        return self

    @dcp
    @abstractmethod
    def read_lock(self):
        return self

    @dcp
    @abstractmethod
    def write_lock(self):
        return self

    @dcp
    def where_exists(self, *rset):
        for t in rset:
            self._exists_tables.append( (WhereClause.AND, t) )
        return self

    @abstractmethod
    def update(self, **kwargs):
        pass

    @dcp
    def or_exists(self, *rset):
        for t in rset:
            self._exists_tables.append( (WhereClause.OR, t) )
        return self

    def _query_sql(self):
        params = []
        from_sql = self._from_sql(params)
        select_sql, select_params = self.select_clause.compile(self.db)
        sql = '{select_sql} {from_sql}{lock_sql}'.format(
            select_sql= select_sql,
            from_sql=from_sql,
            lock_sql=self._lock_sql()
        )
        params.extend(select_params)
        return sql, params

    @abstractmethod
    def _lock_sql(self):
        pass

    def _push_exist_sql(self, where_sql, sql, params):
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

    def _from_sql(self, params):
        sql = 'FROM {tablename}'.format(tablename=self.aqm_tablename)
        join_sql = self._join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)
        return self._core_sql(sql, params)

    def _core_sql(self, sql, params):
        where_sql, where_params = self.where_clause.compile(self.db)
        if where_sql:
            sql = '%s %s' % (sql, where_sql)
            params.extend(where_params)

        sql = self._push_exist_sql(where_sql, sql, params)
        group_sql, group_params = self.group_clause.compile(self.db)
        if group_sql:
            sql = '%s %s' % (sql, group_sql)
            params.extend(group_params)

        having_sql, having_params = self.having_clause.compile(self.db)
        if having_sql:
            sql = '%s %s' % (sql, having_sql)
            params.extend(having_params)

        union_sql = self._union_sql(params)
        if union_sql:
            sql = '%s %s' % (sql, union_sql)

        order_sql, order_params = self.order_clause.compile(self.db)
        if order_sql:
            sql = '%s %s' % (sql, order_sql)
            params.extend(order_params)

        limit_and_offset_sql, limit_and_offset_params = self.page_clause.compile()
        if limit_and_offset_sql:
            sql = '%s %s' % (sql, limit_and_offset_sql)
            params.extend(limit_and_offset_params)

        return sql

    def _union_sql(self, params):
        sqls = []
        for u, _all in self.unions:
            sqls.append( 'UNION ALL' if _all else 'UNION' )
            tmp_sql, tmp_params = u._query_sql()
            sqls.append(tmp_sql)
            params.extend(tmp_params)
        return ' '.join(sqls)

    def _join_sql(self, params):
        sqls = []
        for j in self._joins_clauses:
            tmp_sql, tmp_params = j.compile(self.db)
            if tmp_sql:
                sqls.append(tmp_sql)
            params.extend(tmp_params)
        return ' '.join(sqls)

    def insert_getid(self, record=None, **kwargs):
        record = record or {}
        if kwargs: record.update(kwargs)
        insert_clause = InsertClause(self.aqm_tablename)
        sql, params = insert_clause.insert(record).compile(self.db)
        return self.db.execute_lastrowid(sql, *params)

    def insert(self, records=None, **kwargs):
        insert_clause = InsertClause(self.aqm_tablename)
        sql, params = insert_clause.insert(records, **kwargs).compile(self.db)
        return self.db.execute_rowcount(sql, *params)

    # @dcp
    def update(self, **kwargs):
        update_columns, update_params = [], []
        for k, v in kwargs.items():
            update_columns.append('%s = %s' % (self.db.aqm(k), self.paramstyle))
            update_params.append(v)

        sql = 'UPDATE %s' % self.aqm_tablename
        params = [] 
        join_sql = self._join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        params.extend(update_params)

        sql = self._core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    def increase(self, **kwargs):
        return self._in_or_decrease('+', **kwargs)

    def decrease(self, **kwargs):
        return self._in_or_decrease('-', **kwargs)

    @dcp
    def _in_or_decrease(self, flag='+', **kwargs):
        update_columns, update_params = [], []
        for k, v in kwargs.items():
            update_columns.append(
                '{name} = {name} {flag} {paramstyle}'.format(
                    name=self.db.aqm(k), 
                    flag=flag,
                    paramstyle=self.paramstyle
                )
            )
            update_params.append(v)

        params = []
        sql = 'UPDATE %s' % self.aqm_tablename 
        join_sql = self._join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        params.extend(update_params)

        sql = self._core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    @abstractmethod
    def delete(self):
        pass

    @abstractmethod
    def truncate(self):
        pass

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
        return self._func('count', column, distinct)

    def max(self, column, distinct=False):
        return self._func('max', column, distinct)

    def min(self, column, distinct=False):
        return self._func('min', column, distinct)

    def avg(self, column, distinct=False):
        return self._func('avg', column, distinct)

    def sum(self, column, distinct=False):
        return self._func('sum', column, distinct)

    def _func(self, func_name, column, distinct=False):
        func_name = func_name.upper()

        if not column:
            column = '*'

        if column == '*':
            distinct = False

        column_name = self.db.aqm(column)

        params = []
        from_sql = self._from_sql(params)
        sql = 'SELECT {func_name}({column_name}) AS aggregate {from_sql}'.format(
            func_name=func_name,
            column_name=column_name if not distinct else 'DISTINCT %s' % column_name,
            from_sql=from_sql
        )
        vs = self.db.fetchone(sql, *params)
        return vs.aggregate



class MySQLRecordset(Recordset):

    qutotation = '`'
    paramstyle = '%s'

    @dcp
    def read_lock(self):
        self._lock = self.LOCK.READ
        return self

    @dcp
    def write_lock(self):
        self._lock = self.LOCK.WRITE
        return self

    def truncate(self):
        return self.db.execute_rowcount('TRUNCATE {}'.format(self.aqm_tablename))

    def delete(self):
        if self.model_class:
            # mean that: User.where(age__gt=30).delete()
            self.model_class._delete_relations(self.all())

        params = []
        from_sql = self._from_sql(params)
        if not self._joins_clauses: # needn't join
            sql = "DELETE {from_sql}".format(from_sql=from_sql)
        else:
            sql = "DELETE {tablename} {from_sql}".format(
                tablename=self.aqm_tablename,
                from_sql=from_sql
            )
        return self.db.execute_rowcount(sql, *params)

    # @dcp
    def update(self, **kwargs):
        update_columns, update_params = [], []
        for k, v in kwargs.items():
            update_columns.append('%s = %s' % (self.db.aqm(k), self.paramstyle))
            update_params.append(v)

        sql = 'UPDATE %s' % self.aqm_tablename
        params = [] 
        join_sql = self._join_sql(params)
        if join_sql:
            sql = '%s %s' % (sql, join_sql)

        sql = '%s SET %s' % (sql, ', '.join(update_columns))
        params.extend(update_params)

        sql = self._core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    def _lock_sql(self):
        lock = ''
        if self._lock == self.LOCK.READ:
            lock = ' LOCK IN SHARE MODE'
        elif self._lock == self.LOCK.WRITE:
            lock = ' FOR UPDATE'
        return lock


class SQLiteRecordset(Recordset):

    qutotation = '`'
    paramstyle = '?'        

    def truncate(self):
        r = self.db.execute_rowcount('DELETE FROM {}'.format(self.aqm_tablename))
        find_slite_master_sql = "SELECT `name` FROM `sqlite_master` WHERE `type` = 'table' and `name` = 'sqlite_sequence'"
        if self.db.execute_rowcount(find_slite_master_sql) != -1:
            self.db.execute('UPDATE sqlite_sequence SET seq = 0 where name = {}'.format(self.aqm_tablename))

        return r

    def delete(self):
        if self.model_class:
            # mean that: User.where(age__gt=30).delete()
            self.model_class._delete_relations(self.all())

        params = []
        from_sql = self._from_sql(params)
        if not self._joins_clauses: # needn't join
            sql = "DELETE {from_sql}".format(from_sql=from_sql)
        elif self.table_pk:
            sql = "DELETE FROM {tablename} WHERE {tablename}.{pk} IN (SELECT {tablename}.{pk} {from_sql})".format(
                tablename=self.aqm_tablename,
                from_sql=from_sql,
                pk=self.db.aqm(self.table_pk)
            )
        else:
            raise SQLError("SQLite can't support delete with join")
        return self.db.execute_rowcount(sql, *params)

    # @dcp
    def update(self, **kwargs):
        update_columns, params = [], []
        for k, v in kwargs.items():
            update_columns.append('%s = %s' % (self.db.aqm(k), self.paramstyle))
            params.append(v)

        sql = 'UPDATE %s SET %s' % (self.aqm_tablename, ', '.join(update_columns))
        join_sql = self._join_sql(params)
        if join_sql:
            join_sql = self._core_sql(join_sql, params)
            sub_select_sql = 'WHERE {tablename}.{pk} IN (SELECT {tablename}.{pk} FROM {tablename} {join_sql})'.format(
                tablename=self.aqm_tablename,
                join_sql=join_sql,
                pk=self.db.aqm(self.table_pk)
            )
            sql = '%s %s' % (sql, sub_select_sql)
        else:
            sql = self._core_sql(sql, params)

        return self.db.execute_rowcount(sql, *params)

    def _lock_sql(self):
        return ''

    def read_lock(self):
        return self

    def write_lock(self):
        return self
