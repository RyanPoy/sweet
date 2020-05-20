#coding: utf8
from .model import Model
import functools


def atomic(func_or_db=None):

    def wrapper(func):

        @functools.wraps(func)
        def _(*args, **kwargs):
            t = None
            try:
                with db.transaction():
                    return func(*args, **kwargs)
            except Exception as ex:
                if t:
                    t.rollback()
                raise
            finally:
                db.set_autocommit()
        return _

    if callable(func_or_db):
        db = Model.db
    elif func_or_db is None:
        db = Model.db
    else:
        db = func_or_db

    if callable(func_or_db):
        return wrapper(func_or_db)

    return wrapper


all = [
    Model, atomic
]
