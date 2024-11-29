#coding: utf8
from sweet.utils import is_array, is_hash
from sweet.database.filters import FilterBuilder


class WhereClause:
    
    AND, OR = 'AND', 'OR'

    PREFIX = 'WHERE'

    def __init__(self):
        self.filters = []

    def and_(self, *other_clauses, **kwargs):
        return self.__and_or(self.AND, *other_clauses, **kwargs)

    def or_(self, *other_clauses, **kwargs):
        return self.__and_or(self.OR, *other_clauses, **kwargs)

    def __and_or(self, and_or, *other_clauses, **kwargs):
        for c in other_clauses:
            if not isinstance(c, WhereClause):
                raise TypeError("Must be a Clause type")
            self.filters.append( (and_or, c) )

        for k, v in kwargs.items():
            self.filters.append( (and_or, FilterBuilder.build(k, v)) )
        return self

    def _ltrip_and_or(self, s):
        if s.startswith(self.AND):
            s = s[4:]
        elif s.startswith(self.OR):
            s = s[3:]
        return s

    def compile(self, db):
        sql, params = self._compile(db)
        sql = self._ltrip_and_or(sql)
        if sql:
            sql = '%s %s' % (self.PREFIX, sql)
        return sql, params

    def _compile(self, db):
        sqls, params = [], []
        for and_or, f in self.filters:
            if isinstance(f, WhereClause):
                tmp_sql, tmp_params = f._compile(db)
                tmp_sql = self._ltrip_and_or(tmp_sql)
                sqls.append(and_or)
                sqls.append("(")
                sqls.append(tmp_sql)
                sqls.append(")")
                params.extend(tmp_params)
            else:
                tmp_sql, tmp_params = f.compile(db)
                sqls.append(and_or)
                sqls.append(tmp_sql)
                params.extend(tmp_params)

        return ' '.join(sqls) if sqls else '', params


class HavingClause(WhereClause):
    
    PREFIX = 'HAVING'


######################################
###
class JoinClause(WhereClause):
    
    PREFIX = 'INNER'

    def __init__(self, tbname):
        super().__init__()
        self.tbname = tbname
        self._ons = []

    def on(self, *ons):
        return self._on(self.AND, *ons)

    def or_on(self, *ons):
        return self._on(self.OR, *ons)

    def _on(self, and_or, *ons):
        for on in ons:
            self._ons.append((and_or, on))
        return self

    def compile(self, db):
        sql, params = self._compile(db)

        ons = []
        for and_or, on in self._ons:
            s = ' = '.join([ db.aqm(x.strip()) for x in on.split('=', 1) ])
            ons.append(and_or)
            ons.append(' ')
            ons.append(s)

        on = self._ltrip_and_or(' '.join(ons).strip()).strip()

        tablname = db.aqm(self.tbname)
        if on and sql:
            sql = '%s JOIN %s ON %s %s' % (self.PREFIX, tablname, on, sql)
        elif on:
            sql = '%s JOIN %s ON %s' % (self.PREFIX, tablname, on)
        elif sql:
            sql = self._ltrip_and_or(sql)
            sql = '%s JOIN %s ON %s' % (self.PREFIX, tablname, sql)
        else:
            sql = '%s JOIN %s' % (self.PREFIX, tablname)
        return sql, params


class LeftJoinClause(JoinClause):

    PREFIX = 'LEFT'


class RightJoinClause(JoinClause):

    PREFIX = 'RIGHT'


class CrossJoinClause(JoinClause):

    PREFIX = 'CROSS'


class OrderClause:

    def __init__(self):
        self._bys = []

    def by(self, column, desc=False):
        self._bys.append((column, desc))
        return self

    def compile(self, db):
        if not self._bys:
            return '', []

        bys = []
        for column, desc in self._bys:
            c = db.aqm(column)
            if desc:
                bys.append('%s DESC' % c)
            else:
                bys.append('%s' % c)
        return 'ORDER BY %s' % ', '.join(bys), []


class GroupClause:

    def __init__(self):
        self._bys = []

    def by(self, *columns):
        for c in columns:
            self._bys.append(c)
        return self

    def compile(self, db):
        if not self._bys:
            return '', []

        return 'GROUP BY %s' % ', '.join([ db.aqm(by) for by in self._bys ]), []


class PageClause:

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
class SelectClause:

    def __init__(self):
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

    def compile(self, db):
        sql = '*'
        if self.columns:
            sql = ', '.join([ db.aqm(c) for c in self.columns ])

        if self._distinct:
            sql = 'SELECT DISTINCT %s' % sql
        else:
            sql = 'SELECT %s' % sql
        return sql, []


#################################
###
class InsertClause:
    
    def __init__(self, tbname):
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

    def compile(self, db):
        if not self.list_records:
            return '', [] # nothing insert

        if len(self.list_records) > 1:
            keys = self.list_records[0].keys()
            for r in self.list_records:
                if r.keys() != keys:
                    raise Exception("multiple insert only support same keys records")

        values_sql, params = [], []
        for r in self.list_records:
            values_sql.append('(%s)' % ', '.join([db.paramstyle]*len(r)))
            params.extend(r.values())
        
        sql = 'INSERT INTO {tablename} ({columns}) VALUES {values_sql}'.format(
            tablename=db.aqm(self.tbname),
            columns=', '.join([ db.aqm(c) for c in self.list_records[0].keys() ]),
            values_sql=', '.join(values_sql)
        )
        return sql, params
