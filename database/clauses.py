#coding: utf8
from sweet.utils import is_array, is_str


aqm = lambda s, qutotation: s if s == '*' or not s else '.'.join([ '%s%s%s' % (qutotation, x.strip(qutotation), qutotation) for x in s.split('.') ])


class Filter(object):

    SPLIT_TAG = '__'

    EQ, IN, NOT_IN = '=', 'IN', 'NOT IN'
    IS, IS_NOT = 'IS', 'IS NOT'
    LIKE, NOT_LIKE = 'LIKE', 'NOT LIKE'
    BETWEEN, NOT_BETWEEN = 'BETWEEN', 'NOT BETWEEN'
    not_suffix, bt_suffix, not_bt_suffix = 'not', 'bt', 'not_bt'

    SPECIALS = {
        'like':     LIKE,
        'not_like': NOT_LIKE,
        bt_suffix:     BETWEEN,
        not_bt_suffix: NOT_BETWEEN, # not between
        'gt':       '>', 
        'not_gt':   '<=',
        'gte':      '>=',
        'not_gte':  '<', 
        'lt':       '<', 
        'not_lt':   '>=', 
        'lte':      '<=', 
        'not_lte':  '>',
        'not':      '!=',
    }

    def __init__(self, name, value, qutotation, paramstyle):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self._parse_value(value) # must before _parse_name
        self._parse_name(name)

    def should_ignore_none_value(self):
        return self.operator == self.IS or self.operator == self.IS_NOT

    def _parse_value(self, value):
        self.value = value
        if is_str(self.value) and '.' in self.value:
            self.value = aqm(self.value, self.qutotation)
        return self

    def _parse_name(self, name):
        vs = name.split(self.SPLIT_TAG)
        suffix = vs[-1]
        if suffix in self.SPECIALS:
            if suffix == self.bt_suffix or suffix == self.not_bt_suffix:
                self.operator = self.SPECIALS[suffix]
                self.name = name[:-len(suffix)-2]
            # elif is_array(self.value):
            #     self.operator = self.NOT_IN
            #     self.name = name[:-len(suffix)-2]
            # elif self.value is None:
            #     self.operator = self.IS_NOT
            #     self.name = name[:-len(suffix)-2]
            elif suffix == self.not_suffix: # not
                if is_array(self.value): # not in
                    self.operator = self.NOT_IN
                    self.name = name[:-len(suffix)-2]
                elif self.value is None: # is not
                    self.operator = self.IS_NOT
                    self.name = name[:-len(suffix)-2]
                else:
                    self.operator = self.SPECIALS[suffix]
                    self.name = name[:-len(suffix)-2]
            else:
                self.operator = self.SPECIALS[suffix]
                self.name = name[:-len(suffix)-2]
        elif is_array(self.value): # 如果不在特殊的后缀里面
            self.operator = self.IN
            self.name = name
        elif self.value is None:
            self.operator = self.IS
            self.name = name
        else:
            self.operator = self.EQ
            self.name = name
        self.name = self.name.replace(self.SPLIT_TAG, '.')
        return self

    def to_sql(self):
        name = aqm(self.name, self.qutotation)

        if self.operator == self.IN or self.operator == self.NOT_IN:
            param_str = ', '.join([self.paramstyle]*len(self.value))
            return '{name} {operator} ({param_str})'.format(
                qutotation=self.qutotation, name=name, 
                operator=self.operator, param_str=param_str
            )
        elif self.operator == self.BETWEEN or self.operator == self.NOT_BETWEEN:
            if not is_array(self.value) or len(self.value) != 2:
                raise TypeError("%s just support a array which has 2 elements" )
            return '{name} {operator} {paramstyle} AND {paramstyle}'.format(
                qutotation=self.qutotation, name=name,
                operator=self.operator, paramstyle=self.paramstyle
            )
        elif self.should_ignore_none_value():
            return '{name} {operator} NULL'.format(
                qutotation=self.qutotation, name=name, operator=self.operator
            )
        return '{name} {operator} {paramstyle}'.format(
            qutotation=self.qutotation, name=name, 
            operator=self.operator, paramstyle=self.paramstyle
        )


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
            self.filters.append( (self.AND, Filter(k, v, self.qutotation, self.paramstyle)) )
        return self

    def or_(self, **kwargs):
        for k, v in kwargs.items():
            self.filters.append( (self.OR, Filter(k, v, self.qutotation, self.paramstyle)) )
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
            sqls.append(and_or)
            sqls.append(f.to_sql())
            if is_array(f.value):
                self.bindings.extend(f.value)
            elif not f.should_ignore_none_value():
                self.bindings.append(f.value)
        return ' '.join(sqls) if sqls else ''


class HavingClause(WhereClause):
    
    PREFIX = 'HAVING'


class JoinClause(WhereClause):
    
    PREFIX = 'INNER JOIN'

    def __init__(self, qutotation, paramstyle, tbname):
        super().__init__(qutotation, paramstyle)
        self.tbname = tbname
        self._ons = []

    def on(self, *ons):
        return self._add_on(self.AND, *ons)

    def or_on(self, *ons):
        return self._add_on(self.OR, *ons)        

    def _add_on(self, and_or, *ons):
        for on in ons:
            s = ' = '.join([ aqm(x.strip(), self.qutotation) for x in on.split('=') ])
            # self._ons.append( (and_or, s))
            self._ons.append('%s %s' % (and_or, s))
        return self

    def compile(self):
        s = self._compile()
        on = self._ltrip_and_or(' '.join(self._ons).strip())

        if on and s:
            self.sql = '%s %s ON %s %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on, s)
        elif on:
            self.sql = '%s %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), on)
        elif s:
            s = self._ltrip_and_or(s)
            self.sql = '%s %s ON %s' % (self.PREFIX, aqm(self.tbname, self.qutotation), s)
        else:
            self.sql = '%s %s' % (self.PREFIX, aqm(self.tbname, self.qutotation))
        return self


class LeftJoinClause(JoinClause):

    PREFIX = 'LEFT JOIN'


class RightJoinClause(JoinClause):

    PREFIX = 'RIGHT JOIN'
