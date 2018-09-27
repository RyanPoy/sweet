#coding: utf8
from sweet.utils import is_array

aqm = lambda s, qutotation: s if s == '*' else '.'.join([ '%s%s%s' % (qutotation, x, qutotation) for x in s.split('.') ])

class Filter(object):
    SPLIT_TAG = '__'

    EQ, IN, NOT_IN = '=', 'IN', 'NOT IN'
    IS, IS_NOT = 'IS', 'IS NOT'
    LIKE, NOT_LIKE = 'LIKE', 'NOT LIKE'
    BETWEEN, NOT_BETWEEN = 'BETWEEN', 'NOT BETWEEN'
    bt_suffix, not_bt_suffix = 'bt', 'not_bt'

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

    def __init__(self, name, value):
        self.value = value
        self._parse(name)

    def _parse(self, name):
        vs = name.split(self.SPLIT_TAG)
        suffix = vs[-1]
        if suffix in self.SPECIALS:
            if suffix == self.bt_suffix or suffix == self.not_bt_suffix:
                self.operator = self.SPECIALS[suffix]
                self.name = self.name = name[:-len(suffix)-2]
            elif is_array(self.value):
                self.operator = self.NOT_IN
                self.name = name[:-len(suffix)-2]
            elif self.value is None:
                self.operator = self.IS_NOT
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

    def to_sql(self, qutotation, paramstyle):
        name = aqm(self.name, qutotation)

        if self.operator == self.IN or self.operator == self.NOT_IN:
            param_str = ', '.join([paramstyle]*len(self.value))
            return '{name} {operator} ({param_str})'.format(
                qutotation=qutotation, name=name, 
                operator=self.operator, param_str=param_str
            )
        elif self.operator == self.BETWEEN or self.operator == self.NOT_BETWEEN:
            if not is_array(self.value) or len(self.value) != 2:
                raise TypeError("%s just support a array which has 2 elements" )
            return '{name} {operator} {paramstyle} AND {paramstyle}'.format(
                qutotation=qutotation, name=name,
                operator=self.operator, paramstyle=paramstyle
            )
        return '{name} {operator} {paramstyle}'.format(
            qutotation=qutotation, name=name, 
            operator=self.operator, paramstyle=paramstyle
        )


class WhereClause(object):
    
    AND, OR = 'AND', 'OR'

    def __init__(self, qutotation, paramstyle):
        self.qutotation = qutotation
        self.paramstyle = paramstyle
        self.filters = []
        self.sql = ''
        self.bindings = []

    def and_(self, **kwargs):
        for k, v in kwargs.items():
            self.filters.append( (self.AND, Filter(k, v)) )
        return self

    def or_(self, **kwargs):
        for k, v in kwargs.items():
            self.filters.append( (self.OR, Filter(k, v)) )
        return self

    def compile(self):
        s = self._compile()
        if s:
            self.sql = 'WHERE %s' % s
        return self

    def _compile(self):
        sqls = []
        for and_or, f in self.filters:
            sqls.append(and_or)
            sqls.append(f.to_sql(self.qutotation, self.paramstyle))
            if is_array(f.value):
                self.bindings.extend(f.value)
            else:
                self.bindings.append(f.value)

        if not sqls:
            s = ''
        else:
            s = ' '.join(sqls)
            if s.startswith(self.AND):
                s = s[4:]
            elif s.startswith(self.OR):
                s = s[3:]
        return s


class HavingClause(WhereClause):

    def compile(self):
        s = self._compile()
        if s:
            self.sql = 'HAVING %s' % s
        return self


class JoinOnClause(WhereClause):
    
    def __init__(self, qutotation, paramstyle, tbname):
        super().__init__(qutotation, paramstyle)
        self.tbname = tbname

    def compile(self):
        s = self._compile()
        if s:
            self.sql = 'JOIN %s ON %s' % (aqm(self.tbname), s)
        return self


