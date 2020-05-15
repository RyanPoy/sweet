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


class Collection(list):

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
