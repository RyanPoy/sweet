#coding: utf8
from sweet.utils import is_array
from abc import abstractmethod

class Filter:

    def __init__(self, name, operator, value):
        self.name = name
        self.operator = operator
        self.value = value
        self.valid()

    def valid(self):
        return True

    @abstractmethod
    def compile(self, db):
        pass


class JoinOnFilter(Filter):

    def compile(self, db):
        pass


class SimpleFilter(Filter):
    
    def compile(self, db):
        params = []
        sql = '%s %s %s' % (db.aqm(self.name), self.operator, db.paramstyle)
        params.append(self.value)
        return sql, params


class InFilter(Filter):

    def compile(self, db):
        params = []
        ps = ', '.join([db.paramstyle]*len(self.value))
        sql = '%s %s (%s)' % (db.aqm(self.name), self.operator, ps)
        params.extend(self.value)
        return sql, params


class NullFilter(Filter):

    def compile(self, db):
        return '%s %s NULL' % (db.aqm(self.name), self.operator), []


class BetweenFilter(Filter):
    
    def compile(self, db):
        params = []
        sql = '%s %s %s AND %s' % (db.aqm(self.name), self.operator, db.paramstyle, db.paramstyle)
        params.append(self.value[0])
        params.append(self.value[1])
        return sql, params

    def valid(self):
        if not is_array(self.value) or len(self.value) != 2:
            raise TypeError("%s just support a array which has 2 elements" % self.value) 
        return True


class FilterBuilder:

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
    def build(cls, name, value):
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

        name = name.replace(flag, '.')
        return expression_class(name, operator, value)
