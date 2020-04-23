#coding: utf8
from collections import namedtuple

_field_types = [
    'string', 'text', 'blob',
    'bool', 'int', 'float', 
    'decimal', 'date', 'datetime',  
]
FieldType = namedtuple('FieldType', field_types)._make(field_types)


class Field(object):

    @classmethod
    def mapping(cls):
        if not hasattr(cls, 'MAPPING'):
            setattr(cls, 'MAPPING', {
                FieldType.string: CharField, 
                FieldType.text: TextField, 
                FieldType.blob: BlobField,
                FieldType.bool: BoolField, 
                FieldType.int: IntField, 
                FieldType.float: FloatField, 
                FieldType.decimal: DecimalField, 
                FieldType.date: DateField, 
                FieldType.datetime: DatetimeField,
            })
        return cls.MAPPING

    def _init_name(self, name):
        return (name or '').strip().lower()

    def _init_null(self, null):
        return True if null else False

    @classmethod
    def build(cls, name, field_type):
        if field_type not in cls.mapping:
            raise Exception('Can not support %s field type' % field_type)
        return cls.mapping.get(field_type)(name)

    def __str__(self):
        buff = []
        if self.name is not None:       buff.append('name=%s' % self.name)
        if self.type is not None:       buff.append('type=%s' % self.type)
        if self.length is not None:     buff.append('length=%s' % self.length) 
        
        buff.append('null=%s' % self.null)
        if self.default is not None:    buff.append('default=%s' % self.default) 
        if self.precision is not None:  buff.append('precision=%s' % self.precision)
        if self.scale is not None:      buff.append('scale=%s' % self.scale)

        return 'Column[ %s ]' % ', '.join(buff)


class CharField(Field):

    def __init__(self, name, length=None, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else default.strip()
        self.length = length if length and length > 0 else 32


class IntField(Field):

    def __init__(self, name, length=None, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else to_i(default)
        self.length = length if length and length > 0 else 11


class FloatField(Field):

    def __init__(self, name, null=False, default=None, precision=None, scale=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else to_f(default)
        self.precision = precision
        self.scale = scale


class DecimalField(Field):

    def __init__(self, name, null=False, default=None, precision=None, scale=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else to_decimal(default)
        self.precision = precision
        self.scale = scale


class TextField(Field):

    def __init__(self, name, length=None, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.length = length if length and length > 0 else 512
        self.default = None if default is None else str(default).strip()


class DateField(Field):

    def __init__(self, name, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else string_to_date(value)


class DatetimeField(Field):

    def __init__(self, name, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else self.string_to_time(default)


class BlobField(Field):

    def __init__(self, name, length=None, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = None if default is None else binary_to_string(default)
        self.length = length if length and length > 0 else 512


class BoolField(Field):

    def __init__(self, name, null=False, default=None):
        self.name = self._init_name(name)
        self.null = self._init_null(null)
        self.default = 1 if default else 0

