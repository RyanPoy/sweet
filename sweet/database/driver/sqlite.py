import sqlite3

from sweet.database.driver.base import Driver


class SQLite(Driver):

    qutotation = '`'
    paramstyle = '?'

    RECORDSET_CLASS = SQLiteRecordset
    logger = logging.getLogger('SQLite')

    def prepare(self, **db_config):
        """
        kwargs contain:
            dbname,
            user='root',
            password='',
            host='localhost',
            port=3306,
            charset='utf8',
            show_sql=False
        """

        self._db_args = dict(
            database=   db_config.get('dbname'),
        )

    def get_columns(self, table_name):
        list_column_sql = "PRAGMA table_info('%s')" % table_name

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
            kwargs['default'] = c.dflt_value
            kwargs['null'] = False if c.notnull else True
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
        self._conn = sqlite3.connect(**self._db_args)
        self._conn.row_factory = sqlite_row_factory
        self.set_autocommit(True)
        self.execute('PRAGMA foreign_keys = ON;')

    def _cursor(self):
        return self._conn.cursor()

    def set_autocommit(self, auto=True):
        if auto is True:
            self._conn.isolation_level = None
        else:
            self._conn.isolation_level = 'DEFERRED'
        return self


def sqlite_row_factory(cursor, row):
    r = {}
    for idx, col in enumerate(cursor.description):
        colname = col[0]
        if colname not in r:
            r[colname] = row[idx]
    return r

