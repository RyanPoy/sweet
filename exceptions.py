# -*- coding:utf-8 -*-
class RecordNotFound(Exception):
    pass


class RecordHasNotBeenPersisted(Exception):
    pass


class RecordValidateError(Exception):
    
    pass


class ColumnExistError(Exception):
    pass


class UnsupportAssociation(Exception):
    pass
