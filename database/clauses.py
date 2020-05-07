#coding: utf8
from sweet.utils import is_array, is_str, is_hash
from sweet.database.filters import Filter

aqm = lambda s, qutotation : s if s == '*' or not s \
                                else '.'.join([ '%s%s%s' % (qutotation, x.strip(qutotation), qutotation) for x in s.split('.') ])


class Clause(object):
    pass


class WhereClause(Clause):
    
    AND, OR = 'AND', 'OR'

    PREFIX = 'WHERE'

    def __init__(self, qutotation, paramstyle):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self.filters = []

    def and_(self, *other_clauses, **kwargs):
        return self.__and_or(self.AND, *other_clauses, **kwargs)

    def or_(self, *other_clauses, **kwargs):
        return self.__and_or(self.OR, *other_clauses, **kwargs)

    def __and_or(self, and_or, *other_clauses, **kwargs):
        for c in other_clauses:
            if not isinstance(c, Clause):
                raise TypeError("Must be a Clause type")
            self.filters.append( (and_or, c) )

        for k, v in kwargs.items():
            self.filters.append( (and_or, Filter.new(k, v, self.qutotation, self.paramstyle)) )
        return self

    def compile(self, with_prefix=True):
        sql, bindings = self._compile()
        sql = self._ltrip_and_or(sql)
        if sql and with_prefix:
            sql = '%s %s' % (self.PREFIX, sql)
        return sql, bindings

    def _ltrip_and_or(self, s):
        if s.startswith(self.AND):
            s = s[4:]
        elif s.startswith(self.OR):
            s = s[3:]
        return s

    def _compile(self):
        sqls, bindings = [], []
        for and_or, f in self.filters:
            if isinstance(f, Clause):
                tmp_sql, tmp_bindings = f.compile(False)
                sqls.append(and_or)
                sqls.append("(")
                sqls.append(tmp_sql)
                sqls.append(")")
                bindings.extend(tmp_bindings)
            else:
                tmp_sql, tmp_params = f.compile()
                sqls.append(and_or)
                sqls.append(tmp_sql)
                bindings.extend(tmp_params)
        return ' '.join(sqls) if sqls else '', bindings


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
            s = ' = '.join([ aqm(x.strip(), self.qutotation) for x in on.split('=', 1) ])
            self._ons.append('%s %s' % (and_or, s))
        return self

    def compile(self):
        sql, bindings = self._compile()
        on = self._ltrip_and_or(' '.join(self._ons).strip())

        if on and sql:
            sql = '%s JOIN %s ON %s %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on, sql)
        elif on:
            sql = '%s JOIN %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on)
        elif sql:
            sql = self._ltrip_and_or(sql)
            sql = '%s JOIN %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), sql)
        else:
            sql = '%s JOIN %s' % (self.PREFIX, aqm(self.tbname, self.qutotation))
        return sql, bindings


class LeftJoinClause(JoinClause):

    PREFIX = 'LEFT'


class RightJoinClause(JoinClause):

    PREFIX = 'RIGHT'


class CrossJoinClause(JoinClause):

    PREFIX = 'CROSS'


######################################
###
class ByClause(object):

    def __init__(self, qutotation):
        self.qutotation = qutotation
        self._bys = []

    def compile(self):
        if not self._bys:
            return '', []
        return '%s %s' % (self.PREFIX, ', '.join(self._bys)), []


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
        sql = ' '.join(sqls)
        return sql, []


##################################
###
class SelectClause(object):

    def __init__(self, qutotation):
        self.qutotation = qutotation
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

    def compile(self):
        sql = '*'
        if self.columns:
            sql = ', '.join([ aqm(c, self.qutotation) for c in self.columns ])

        if self._distinct:
            sql = 'SELECT DISTINCT %s' % sql
        else:
            sql = 'SELECT %s' % sql
        return sql, []


#################################
###
class InsertClause(object):
    
    def __init__(self, qutotation, paramstyle, tbname):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self.tbname = tbname
        self.list_records = []

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
            return '', [] # nothing insert

        if len(self.list_records) > 1:
            keys = self.list_records[0].keys()
            for r in self.list_records:
                if r.keys() != keys:
                    raise Exception("multiple insert only support same keys records")

        values_sql, bindings = [], []
        for r in self.list_records:
            values_sql.append('(%s)' % ', '.join([self.paramstyle]*len(r)))
            bindings.extend(r.values())
        
        sql = 'INSERT INTO {tablename} ({columns}) VALUES {values_sql}'.format(
            tablename=aqm(self.tbname, self.qutotation),
            columns=', '.join([ aqm(c, self.qutotation) for c in self.list_records[0].keys() ]),
            values_sql=', '.join(values_sql)
        )
        return sql, bindings
