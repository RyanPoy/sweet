from sweet.database.recordset import MySQLRecordset, SQLiteRecordset
import logging


class MySQL(Driver):

    qutotation = '`'
    paramstyle = '%s'

    RECORDSET_CLASS = MySQLRecordset
    logger = logging.getLogger('MySQL')

    def prepare(self, **db_config):
        self._db_args = dict(
            db      =   db_config.get('dbname'),
            user    =   db_config.get('user'),
            passwd  =   db_config.get('password', ''),
            host    =   db_config.get('host', 'localhost'),
            port    =   db_config.get('port', 3306),
            charset =   db_config.get('charset', 'utf8'),
            sql_mode    =   "TRADITIONAL",
            use_unicode =   True,
            init_command =  'SET time_zone = "+0:00"',
        )

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
    TABLE_NAME = "%s"''' % (self._db_args['database'], table_name)

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

    def _reconnect(self):
        self.close()
        self._conn = MySQLdb.connect(**self._db_args)
        self.set_autocommit(True)

    def _cursor(self):
        from MySQLdb.cursors import DictCursor
        return self._conn.cursor(DictCursor)

    def set_autocommit(self, auto=True):

        return self

