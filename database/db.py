#coding: utf8
from sweet.utils import *
from contextlib import contextmanager
from sweet.database.recordset import MySQLRecordset
import MySQLdb
import time


class Record(mydict):
    pass


class MySQL(object):
    
    table_class = MySQLRecordset

    def __init__(self, dbname, user='root', password='', host='localhost', port=3306, charset='utf8', show_sql=False):
        self._db_args = dict(db=dbname, user=user, passwd=password, host=host, port=port, charset=charset,
                             init_command='SET time_zone = "+0:00"', sql_mode="TRADITIONAL", use_unicode=True)
        self._conn = None
        self.show_sql = show_sql
        self._reconnect()

    def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        return self.fetchall(sql, *params).first()

    def fetchlastone(self, sql, *params):
        """Returns the last row returned for the given query."""
        return self.fetchall(sql, *params).last()
    
    def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        cursor = self._cursor()
        try:
            self._execute(cursor, sql, *params)
            return Collection([ Record(row) for row in cursor ])
        finally:
            cursor.close()

    def execute_lastrowid(self, sql, *params):
        """Executes the given query, returning the lastrowid from the query."""
        cursor = self._cursor()
        try:
            self._execute(cursor, sql, *params)
            return cursor.lastrowid
        finally:
            cursor.close()
    
    def execute_rowcount(self, sql, *params):
        """Executes the given query, returning the rowcount from the query."""
        cursor = self._cursor()
        try:
            self._execute(cursor, sql, *params)
            return cursor.rowcount
        finally:
            cursor.close()
            
    def execute(self, sql, *params):
        cursor = self._cursor()
        try:
            self._execute(cursor, sql, *params)
            return self
        finally:
            cursor.close()
    raw = execute

    def _execute(self, cursor, sql, *params):
        try:
            btime = time.time()
            cursor.execute(sql, params)
        finally:
            if self.show_sql:
                param_buff = []
                for p in params:
                    if is_str(p):
                        p = "'%s'" % p
                    else:
                        p = str(p)
                    param_buff.append(p)
                print ((time.time() - btime), '\t|', sql, '\t|', ', '.join(param_buff))
        return self

#     def get_table_by(self, name):
#         sql = 'SHOW FULL COLUMNS FROM %s' % name
#         table = Table(name)
#         for field in self.fetchall(sql):
#             table.add_column(Column(
#                 name=field['Field'], type=self.column_type_of(field['Type']), 
#                 null=field['Null'] == "YES", default=field['Default']
# #                field['Collation']
#             ))
#         return table
    
    # def column_type_of(self, db_field_type):
    #     db_field_type = db_field_type.lower().strip().split('(')[0]
    #     for field_type, column_type in { 
    #         'varchar': Column.Type.string, 'text': Column.Type.text, 'tinyint': Column.Type.boolean, 
    #         'float': Column.Type.float, 'decimal': Column.Type.decimal, 'datetime': Column.Type.datetime, 
    #         'date': Column.Type.date, 'timestamp': Column.Type.timestamp, 'blob': Column.Type.binary, 
    #         'int':Column.Type.int, 'longtext': Column.Type.text, }.iteritems():
    #         if db_field_type == field_type:
    #             return column_type
    #     raise Exception('Can not support %s field type !' % db_field_type)

    def _reconnect(self):
        self.close()
        self._conn = MySQLdb.connect(**self._db_args)
        self.set_autocommit(True)
    
    def _cursor(self):
        from MySQLdb.cursors import DictCursor
        return self._conn.cursor(DictCursor)
        # return self._conn.cursor()

    def close(self):
        try:
            if self._conn:
                self._conn.close()
        finally:
            self._conn = None
    
    def __del__(self):
        self.close()

    def commit(self):
        self._conn.commit()
        return self

    def rollback(self):
        self._conn.rollback()
        return self

    @contextmanager
    def transaction(self):
        self.begin_transaction()
        try:
            yield self
        except Exception as e:
            self.rollback()
            raise
        try:
            self.commit()
        except Exception:
            self.rollback()
            raise

    def begin_transaction(self):
        self.set_autocommit(False)
        return self

    def set_autocommit(self, auto=True):
        self._conn.autocommit(auto)
        return self

    def table(self, tbname):
        return self.table_class(self, tbname)
