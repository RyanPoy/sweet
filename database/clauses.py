#coding: utf8
from sweet.utils import is_array, is_str, is_hash
from sweet.database.filters import Filter

aqm = lambda s, qutotation: s if s == '*' or not s else '.'.join([ '%s%s%s' % (qutotation, x.strip(qutotation), qutotation) for x in s.split('.') ])


class WhereClause(object):
    
    AND, OR = 'AND', 'OR'

    PREFIX = 'WHERE'

    def __init__(self, qutotation, paramstyle):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self.filters = []
        self.sql = ''
        self.bindings = []

    def and_(self, **kwargs):
        for k, v in kwargs.items():
            self.filters.append( (self.AND, Filter.new(k, v, self.qutotation, self.paramstyle)) )
        return self

    def or_(self, **kwargs):
        for k, v in kwargs.items():
            self.filters.append( (self.OR, Filter.new(k, v, self.qutotation, self.paramstyle)) )
        return self

    def compile(self):
        s = self._compile()
        s = self._ltrip_and_or(s)
        if s:
            self.sql = '%s %s' % (self.PREFIX, s)
        return self

    def _ltrip_and_or(self, s):
        if s.startswith(self.AND):
            s = s[4:]
        elif s.startswith(self.OR):
            s = s[3:]
        return s

    def _compile(self):
        sqls = []
        for and_or, f in self.filters:
            f.compile()
            sqls.append(and_or)
            sqls.append(f.sql)
            self.bindings.extend(f.params)
        return ' '.join(sqls) if sqls else ''


class HavingClause(WhereClause):
    
    PREFIX = 'HAVING'


######################################
###
class JoinClause(WhereClause):
    
    PREFIX = 'INNER'

    def __init__(self, qutotation, paramstyle, tbname):
        super().__init__(qutotation, paramstyle)
        self.tbname = tbname
        self._ons = []

    def on(self, *ons):
        return self._on(self.AND, *ons)

    def or_on(self, *ons):
        return self._on(self.OR, *ons)        

    def _on(self, and_or, *ons):
        for on in ons:
            s = ' = '.join([ aqm(x.strip(), self.qutotation) for x in on.split('=') ])
            self._ons.append('%s %s' % (and_or, s))
        return self

    @classmethod
    def join(cls, qutotation, paramstyle, tbname, on):
        o = cls(qutotation, paramstyle, tbname)
        o._ons.append(on)
        return self

    def compile(self):
        s = self._compile()
        on = self._ltrip_and_or(' '.join(self._ons).strip())

        if on and s:
            self.sql = '%s JOIN %s ON %s %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on, s)
        elif on:
            self.sql = '%s JOIN %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on)
        elif s:
            s = self._ltrip_and_or(s)
            self.sql = '%s JOIN %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), s)
        else:
            self.sql = '%s JOIN %s' % (self.PREFIX, aqm(self.tbname, self.qutotation))
        return self


class LeftJoinClause(JoinClause):

    PREFIX = 'LEFT'


class RightJoinClause(JoinClause):

    PREFIX = 'RIGHT'


######################################
###
class ByClause(object):

    def __init__(self, qutotation):
        self.qutotation = qutotation
        self._bys = []
        self.sql = ''

    def compile(self):
        if self._bys:
            self.sql = '%s %s' % (self.PREFIX, ', '.join(self._bys))
        return self


class OrderClause(ByClause):

    PREFIX = 'ORDER BY'

    def by(self, column, desc=False):
        c = aqm(column, self.qutotation)
        if desc:
            self._bys.append('%s DESC' % c)
        else:
            self._bys.append(c)
        return self


class GroupClause(ByClause):

    PREFIX = 'GROUP BY'

    def by(self, *columns):
        for c in columns:
            self._bys.append(aqm(c, self.qutotation))
        return self


class PageClause(object):

    def __init__(self):
        self._limit = None
        self._offset = None

    def limit(self, n):
        self._limit = n
        return self

    def offset(self, n):
        self._offset = n
        return self

    def page(self, page_num, page_size):
        page_num = 1 if page_num < 0 else page_num
        return self.limit((page_num-1) * page_size).offset(page_size)

    def compile(self):
        sqls = []
        if self._limit:
            sqls.append('LIMIT %s' % self._limit)
        if self._offset:
            sqls.append('OFFSET %s' % self._offset)
        self.sql = ' '.join(sqls)
        return self


##################################
###
class SelectClause(object):

    def __init__(self, qutotation):
        self.qutotation = qutotation
        self.sql = ''
        self.columns = []
        self._distinct = False

    def distinct(self):
        self._distinct = True
        return self

    def select(self, *columns):
        columns = columns or '*'
        for c in columns:
            self.columns.append(c)
        return self

    def compile(self, tablename=None):
        if tablename:
            tablename = aqm(tablename, self.qutotation)
            sql = '%s.*' % tablename
            if self.columns:
                sql = ', '.join([ '%s.%s' % (tablename, aqm(c, self.qutotation)) for c in self.columns ])
        else:
            sql = '*'
            if self.columns:
                sql = ', '.join([ aqm(c, self.qutotation) for c in self.columns ])

        if self._distinct:
            self.sql = 'SELECT DISTINCT %s' % sql
        else:
            self.sql = 'SELECT %s' % sql
        return self


#################################
###
class InsertClause(object):
    
    def __init__(self, qutotation, paramstyle, tbname):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self.sql = ''
        self.tbname = tbname
        self.list_records = []
        self.bindings = []

    def insert(self, records=None, **kwargs):
        if records:
            if is_hash(records):
                self.list_records.append(records)
            elif is_array(records):
                self.list_records.extend(records)

        if kwargs:
            self.list_records.append(kwargs)

        return self

    def compile(self):
        if not self.list_records:
            return self # nothing insert

        if len(self.list_records) > 1:
            keys = self.list_records[0].keys()
            for r in self.list_records:
                if r.keys() != keys:
                    raise Exception("multiple insert only support same keys records")

        values_sql = []
        for r in self.list_records:
            values_sql.append('(%s)' % ', '.join([self.paramstyle]*len(r)))
            self.bindings.extend(r.values())
        
        self.sql = 'INSERT INTO {tablename} ({columns}) VALUES {values_sql}'.format(
            tablename=aqm(self.tbname, self.qutotation),
            columns=', '.join([ aqm(c, self.qutotation) for c in self.list_records[0].keys() ]),
            values_sql=', '.join(values_sql)
        )
        return self


