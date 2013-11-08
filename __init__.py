#coding: utf8
from pyrails.db import get_database


def create_table(sql):
    db = get_database(show_sql=False)
    db.execute_rowcount(sql)
    db.close()


def drop_table(table_name):
    db = get_database(show_sql=False)
    db.execute_rowcount('drop table if exists %s' % table_name)
    db.close()
