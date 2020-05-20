#coding: utf8
from sweet.database.recordset import MySQLRecordset
from contextlib import contextmanager
from sweet.orm.fields import *
from sweet.utils import *
import MySQLdb
import time
import logging


logger = logging.getLogger('MySQL')


class MySQL(object):
    
    RECORDSET_CLASS = MySQLRecordset

    COLUMN_FIELD_MAPPING = {
        'integer': IntField, 
        'int': IntField, 
        
        'smallint': IntField,
        'bigint': IntField,

        'real': FloatField, 
        'double': FloatField,
        'numeric': DecimalField,
        'decimal': DecimalField,

        'varchar': CharField,
        'char': CharField,

        'bool': BoolField, 
        'tinyint': BoolField,

        'blob': BlobField,
        'varbinary': BlobField,

        'text': TextField,
        
        'date': DateField, 
        'datetime': DatetimeField,
        'time': DatetimeField,
    }

    def __init__(self, dbname, user='root', password='', host='localhost', port=3306, charset='utf8', show_sql=False):
        self._db_args = dict(db=dbname, user=user, passwd=password, host=host, port=port, charset=charset,
                             init_command='SET time_zone = "+0:00"', sql_mode="TRADITIONAL", use_unicode=True)
        self._conn = None
        self.show_sql = show_sql
        self._reconnect()

    def fetchone(self, sql, *params):
        """Returns the first row returned for the given query."""
        vs = self.fetchall(sql, *params)
        return vs[0] if vs else None

    def fetchlastone(self, sql, *params):
        """Returns the last row returned for the given query."""
        vs = self.fetchall(sql, *params)
        return vs[-1] if vs else None
    
    def fetchall(self, sql, *params):
        """Returns a row list for the given query and parameters."""
        cursor = self._cursor()
        try:
            self._execute(cursor, sql, *params)
            return [ mydict(row) for row in cursor ]
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
            if self.show_sql:
                self._log_msg(logger.debug, btime, sql, *params)
        except:
            self._log_msg(logger.error, btime, sql, *params)
            raise
        return self

    def _log_msg(self, log_func, btime, sql, *params):
        param_buff = []
        for p in params:
            if is_str(p):
                p = "'%s'" % p
            else:
                p = str(p)
            param_buff.append(p)
        log_func('%s\t|%s\t|%s', time.time() - btime, sql, ', '.join(param_buff))
        return self

    def get_columns(self, table_name):
        list_column_sql = '''SELECT 
    COLUMN_NAME AS name, 
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
    TABLE_NAME = "%s"''' % (self._db_args['db'], table_name)

        columns = self.fetchall(list_column_sql)
        fields = []
        for c in columns:
            has_length, has_precision = False, False
            length, precision, scale = None, None, None,

            # type_str looks like: 
            #   bigint(20)
            #   decimal(10,0)
            #   blob
            type_str = c.type
            vs = type_str.split('(')
            column_type = vs[0].split()[0]

            if '(' in type_str and ',' in type_str:
                has_precision = True
                precision, scale = vs[1][:-1].split(',') # 10,0

            elif '(' in type_str:
                has_length = True 
                length = vs[1][:-1] # 20

            field_class = self.COLUMN_FIELD_MAPPING.get(column_type, None)
            if field_class is None:
                raise Exception('Can not support %s field type !' % column_type)

            kwargs = {}
            kwargs['name'] = c.name
            kwargs['default'] = c.default
            kwargs['null'] = c.null
            if has_length:
                kwargs['length'] = int(length)
            if has_precision:
                kwargs['precision'] = int(precision)
                kwargs['scale'] = int(scale)

            try:
                fields.append(field_class(**kwargs))
            except:
                # print('?'*10, c.name, field_class, c)
                raise

        return fields


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
        finally:
            self.set_autocommit()

        try:
            self.commit()
        except Exception:
            self.rollback()
            raise
        finally:
            self.set_autocommit()

    def begin_transaction(self):
        self.set_autocommit(False)
        return self

    def set_autocommit(self, auto=True):
        self._conn.autocommit(auto)
        return self

    def records(self, tbname):
        return self.RECORDSET_CLASS(self, tbname)
