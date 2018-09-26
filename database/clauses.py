#coding: utf8
from sweet.utils import is_array


class Filter(object):
    SPLIT_TAG = '__'

    EQ, IN, NOT_IN = '=', 'IN', 'NOT IN'
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
            else:
                self.operator = self.SPECIALS[suffix]
                self.name = name[:-len(suffix)-2]
        elif is_array(self.value): # 如果不在特殊的后缀里面
            self.operator = self.IN
            self.name = name
        else:
            self.operator = self.EQ
            self.name = name
        self.name = self.name.replace(self.SPLIT_TAG, '.')
        return self

    def to_sql(self, qutotation, paramstyle):
        if self.operator == self.IN or self.operator == self.NOT_IN:
            param_str = ', '.join([paramstyle]*len(self.value))
            return '{qutotation}{name}{qutotation} {operator} ({param_str})'.format(
                qutotation=qutotation, name=self.name, 
                operator=self.operator, param_str=param_str
            )
        elif self.operator == self.BETWEEN or self.operator == self.NOT_BETWEEN:
            if not is_array(self.value) or len(self.value) != 2:
                raise Exception("%s just support a array which has 2 elements" )
            return '{qutotation}{name}{qutotation} {operator} {paramstyle} AND {paramstyle}'.format(
                qutotation=qutotation, name=self.name,
                operator=self.operator, paramstyle=paramstyle
            )
        return '{qutotation}{name}{qutotation} {operator} {paramstyle}'.format(
            qutotation=qutotation, name=self.name, 
            operator=self.operator, paramstyle=paramstyle
        )




class Clause(object):

    def __init__(self, qutotation, paramstyle):
        self.qutotation = qutotation
        self.paramstyle = paramstyle


class WhereClause(Clause):
    
    PREFIX = 'WHERE'

    def __init__(self, qutotation, paramstyle):
        super().__init__(qutotation, paramstyle)
        self.filters = []
        self.sql = ''
        self.bindings = []

    def and_(**kwargs):
        for k, v in kwargs.items():
            self.filters.append( ('AND', Filter(k, v)) )
        return self

    def or_(**kwargs):
        for k, v in kwargs.items():
            self.filters.append( ('AND', Filter(k, v)) )
        return self

    def compile(self):
        sqls = []
        for and_or, f in self.filters:
            sqls.append(and_or)
            sqls.append(f.sql(f.to_sql(self.qutotation, self.paramstyle)))
            if is_array(f.value):
                self.bindings.extend(f.value)
            else:
                self.bindings.append(f.value)

        if not sqls:
            self.sql = ''
        else:
            self.sql = '%s %s' % (self.PREFIX, ' '.join(sqls))
        return self


class HavingClause(WhereClause):

    PREFIX = 'HAVING'


