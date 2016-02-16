# -*- coding:utf-8 -*-
from mock.mock import self

def join_select(selects):
    new_selects = []
    for select in selects:
        select_lower = select.lower()
        if select == '*':
            new_selects.append(select)
        elif ' as ' in select_lower: # aaa.bbb as a.b, ccc.ddd as c.d
            new_selects.append(
                ', '.join([ 
                    ' AS '.join([ 
                        '.'.join([ 
                            '`%s`' % part for part in parts.split('.') # [aaa, bbb]
                        ]) for parts in segment.strip().split(' as ') # [ aaa.bbb, a.b ]
                    ]) for segment in select_lower.split(',') # [ aaa.bbb as a.b, ccc.ddd as c.d ]
                ])
            )
        else:
            new_selects.append('`%s`' % select)
    return ', '.join(new_selects)


class QueryBuilder(object):

    def __init__(self):
        self._selects = []
        self._distinct = False
        self._from = None

    def select(self, *select_columns):
        self._selects += select_columns
        return self

    def from_(self, tablename):
        self._from = tablename
        return self
    
    def distinct(self):
        self._distinct = True
        return self

    def to_sql(self):
        if self._distinct:
            return 'SELECT DISTINCT %s FROM `%s`' % ( join_select(self._selects), self._from)
        else:
            return 'SELECT %s FROM `%s`' % ( join_select(self._selects), self._from)

