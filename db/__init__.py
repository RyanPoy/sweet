#coding: utf-8
from pyrails.activesupport import import_object
import copy

DEBUG = True
DATABASE = dict (
    engine      = 'mysql',
    database    = 'pyrails',
    user        = 'root', 
    password    = '', 
    host        = 'localhost', 
    port        =  3306, 
    charset     =  'utf8',
)


def get_database(db_settings=DATABASE, show_sql=DEBUG):
    engine_name = db_settings.get('engine').lower()
    adapter_class = import_object('pyrails.db.%s.Adapter' % engine_name)
    settings = copy.copy(db_settings)
    settings.pop('engine')
    settings['show_sql'] = show_sql
    return adapter_class(**settings)
