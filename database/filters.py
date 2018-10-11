#coding: utf8
from sweet.utils import is_array


class Filter(object):

    def __init__(self, name, operator, value):
        self.name = name
        self.operator = operator
        self.value = value
        self.valid()
        self.sql = ''
        self.params = []
        self.qutotation = '`'
        self.paramstyle = '?'

        self.valid()

    @classmethod
    def mapping(cls):
        if not hasattr(cls, 'MAPPING'):
            setattr(cls, 'MAPPING', {
                'bt':       ('BETWEEN', BetweenFilter),
                'not_bt':   ('NOT BETWEEN', BetweenFilter),
                'like':     ('LIKE', SimpleFilter),
                'not_like': ('NOT LIKE', SimpleFilter),
                'gt':       ('>', SimpleFilter),
                'gte':      ('>=', SimpleFilter),
                'lt':       ('<', SimpleFilter),
                'lte':      ('<=', SimpleFilter),
                'not':      None, # 不固定
            })
        return cls.MAPPING

    @classmethod
    def new(cls, name, value, qutotation, paramstyle):
        flag = '__'
        vs = name.split(flag)
        suffix = vs[-1]

        suffix_mapping = cls.mapping()
        if suffix in suffix_mapping:
            name = flag.join(vs[:-1])
            if suffix == 'not':
                if value is None:
                    operator = 'IS NOT'
                    expression_class = NullFilter
                elif is_array(value):
                    operator = 'NOT IN'
                    expression_class = InFilter
                else:
                    operator = '<>'
                    expression_class = SimpleFilter
            else:
                operator, expression_class = suffix_mapping[suffix]
        else:
            if value is None:
                operator = 'IS'
                expression_class = NullFilter
            elif is_array(value):
                operator = 'IN'
                expression_class = InFilter
            else:
                operator = '='
                expression_class = SimpleFilter

        expression = expression_class(name, operator, value)
        expression.qutotation = qutotation
        expression.paramstyle = paramstyle
        expression.name = name.replace(flag, '.')
        return expression

    def aqm(self, s): 
        if s == '*' or not s:
            return s
        q = self.qutotation
        return '.'.join([ '%s%s%s' % (q, x.strip(q), q) for x in s.split('.') ])

    @property
    def aqm_name(self):
        if not hasattr(self, '_aqm_name'):
            setattr(self, '_aqm_name', self.aqm(self.name))
        return getattr(self, '_aqm_name')

    def valid(self):
        return True


class SimpleFilter(Filter):
    
    def compile(self):
        self.sql = '%s %s %s' % (self.aqm_name, self.operator, self.paramstyle)
        self.params.append(self.value)
        return self


class InFilter(Filter):

    def compile(self):
        ps = ', '.join([self.paramstyle]*len(self.value))
        self.sql = '%s %s (%s)' % (self.aqm_name, self.operator, ps)
        self.params.extend(self.value)
        return self


class NullFilter(Filter):

    def compile(self):
        self.sql = '%s %s NULL' % (self.aqm_name, self.operator)
        return self


class BetweenFilter(Filter):
    
    def compile(self):
        self.sql = '%s %s %s AND %s' % (self.aqm_name, self.operator, self.paramstyle, self.paramstyle)
        self.params.append(self.value[0])
        self.params.append(self.value[1])
        return self

    def valid(self):
        if not is_array(self.value) or len(self.value) != 2:
            raise TypeError("%s just support a array which has 2 elements" % self.value) 
        return True



