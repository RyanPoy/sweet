#coding: utf8

value = lambda val: val() if callable(val) else val


def get_value(item, key, default=None):
    attrs = key.split('.')
    relt = None
    data = item
    while attrs:
        key = attrs.pop(0)
        if hasattr(data, key):
            data = getattr(data, key)
            relt = data
        else:
            relt = None
            break
    return relt


class MemCollection(list):

    def __init__(self, *items):
        self.items = items or []

    def first(self):
        return self.items[0]

    def last(self):
        return self.items[-1]

    def all(self):
        return self.items

    def __len__(self):
        return len(self.items)

    def len(self):
        return len(self.items)

    def count(self):
        return len(self.items)

    def max(self, column):
        if self.is_empty():
            raise ValueError('max() arg is an empty sequence')

        max_v = self.items[0]
        for item in self.items:
            v = get_value(item, column)
            if v is not None and max_v < v:
                max_v = v
        return max_v

    def min(self, column):
        if self.is_empty():
            raise ValueError('min() arg is an empty sequence')

        min_v = self.items[0]
        for item in self.items:
            v = get_value(item, column)
            if v is not None and min_v > v:
                min_v = v
        return min_v

    def sum(self, column):
        if self.is_empty():
            return 0
        sum_v = 0
        for item in self.items:
            v = get_value(item, column)
            if v is not None:
                sum_v += v
        return sum_v

    def avg(self, key=None):
        count = self.count()
        if count:
            return self.sum(key) / count

    def __iter__(self):
        return iter(self.items)

    def is_empty(self):
        return len(self) == 0

    def to_json(self, **options):
        return json.dumps(self.items, **options)



class Collection:

    def __init__(self, rs):
        self.rs = rs

    def distinct(self):
        return self.rs.distinct()

    def select(self, *columns):
        return self.rs.select(*columns)

    def where(self, *where_clauses, **kwargs):
        return self.rs.where(*where_clauses, **kwargs)

    def or_where(self, *where_clauses, **kwargs):
        return self.rs.or_where(*where_clauses, **kwargs)

    def having(self, *where_clauses, **kwargs):
        return self.rs.having(*where_clauses, **kwargs)

    def or_having(self, *where_clauses, **kwargs):
        return self.rs.or_having(*where_clauses, **kwargs)

    def group_by(self, *columns):
        return self.rs.group_by(*columns)

    def order_by(self, column, desc=False):
        return self.rs.order_by(column, desc)

    def limit(self, num):
        return self.rs.limit(num)

    def offset(self, num):
        return self.rs.offset(num)

    def page(self, page_num, page_size):
        return self.rs.page(page_num, page_size)

    def join(self, tbname, on=None, func=None):
        return self.rs.join(tbname, on, func)

    def left_join(self, tbname, on=None, func=None):
        return self.rs.left_join(tbname, on, func)

    def cross_join(self, tbname, on=None, func=None):
        return self.rs.cross_join(tbname, on, func)

    def right_join(self, tbname, on=None, func=None):
        return self.rs.right_join(tbname, on, func)

    def read_lock(self):
        return self.rs.read_lock()

    def write_lock(self):
        return self.rs.write_lock()

    def where_exists(self, *rset):
        return self.rs.where_exists(*rset)

    def update(self, **kwargs):
        return self.rs.update(**kwargs)

    def or_exists(self, *rset):
        return self.rs.or_exists(*rset)

    def insert_getid(self, record=None, **kwargs):
        return self.rs.insert_getid(record, **kwargs)

    def insert(self, records=None, **kwargs):
        return self.insert(records, **kwargs)

    def update(self, **kwargs):
        return self.rs.update(**kwargs)

    def increase(self, **kwargs):
        return self.rs.increase(**kwargs)

    def decrease(self, **kwargs):
        return self.rs.decrease(**kwargs)

    def delete(self):
        return self.rs.delete()

    def truncate(self):
        return self.rs.truncate()

    def first(self):
        return self.rs.first()

    def last(self):
        return self.rs.last()

    def all(self):
        return self.rs.all()

    def exists(self):
        return self.rs.exists()

    def count(self, column=None, distinct=False):
        return self.rs.count(column, distinct)

    def max(self, column, distinct=False):
        return self.rs.max(column, distinct)

    def min(self, column, distinct=False):
        return self.rs.min(column, distinct)

    def avg(self, column, distinct=False):
        return self.rs.avg(column, distinct)

    def sum(self, column, distinct=False):
        return self.rs.sum(column, distinct)
