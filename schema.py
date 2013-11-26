#coding: utf8

# The MIT License (MIT)
#
# Copyright (c) 2013 PengYi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
from collections import namedtuple
from pyrails.activesupport import ColumnExistError, to_bool, to_i, to_f, to_decimal, str2datetime, str2date, binary2str


column_types = [
    'string', 'text', 'int', 'float', 'decimal', 
    'date', 'datetime', 'timestamp', 'binary', 'boolean'
]


class Column(object):
    
    Type = namedtuple('Type', column_types)._make(column_types)

    def __init__(self, name=None, type=None, length=None, null=False, default=None, precision=None, scale=None):
        """
        Instantiates a new column in the table.
        @param name:  is the column's name, such as <tt>supplier_id</tt> in <tt>supplier_id int(11)</tt>.
        @param type:  is the column's type. 
                      All type are: 
                      'string', 'text', 'integer', 'float', 'decimal', 'date', 
                      'time', 'datetime', 'timestamp', 'binary', 'boolean'
        @param length: length of column 
        @param default: is the type-casted default value, such as +new+ in <tt>sales_stage varchar(20) default 'new'</tt>.
        @param decimals: is only used to extract the column's length, if necessary. For example +60+ in <tt>company_name varchar(60)</tt>.
        @param null: determines if this column allows +NULL+ values.
        """
        self.name       = self.__extract_name(name)
        self.type       = self.__extract_type(type)
        self.null       = self.__extract_null(null)
        self.default    = self.__extract_default(default)
        self.length     = self.__extract_length(length)
        
        self.precision, self.scale  = self.__extract_precison_and_scale(precision, scale)
        
        self.primary = None
        
    def __extract_name(self, name):
        if name:        name = name.strip()
#        if not name:    raise Exception('Name can not a empty value.')
        if not name: name = ''
        return name.lower()
    
    def __extract_type(self, type):
        if type not in set(Column.Type._fields):
            raise Exception('%s is a support column type' % type)
        return type
            
    def __extract_null(self, null):
        if null is not None:
            null = to_bool(null)
        return null
    
    def __extract_default(self, default):
        if self.is_boolean(): 
            return 1 if default else 0
        if self.is_lob():
            if default is not None:
                raise Exception('%s type column can not support default.' % self.type)
            
        return self.type_cast(default)
        
    def __extract_length(self, length):
        if not (self.type == Column.Type.string or self.type == Column.Type.text or  
                self.type == Column.Type.binary or self.type == Column.Type.int):
            if length: 
                raise Exception('%s type column can not support length.' % self.type)
        elif self.type == Column.Type.int:
            if length is not None and length <= 0:
                raise Exception('Length must greater than 0')
        else:
            if not length:  
                length = 32 # default length value
            if length <= 0:  
                raise Exception('Length must greater than 0')
        return length
    
    def __extract_precison_and_scale(self, precision, scale):
        if not (self.type == Column.Type.float or self.type == Column.Type.decimal):
            if precision or scale: 
                raise Exception('%s type column can not support precision, scale.' % self.type)
        else:
            if precision and precision < 0:
                raise Exception('precision must greater than 0')
            if scale and scale < 0:
                raise Exception('scale must greater than 0')
            if precision < scale:
                raise Exception('precision must greater than scale') 
        return precision, scale    
        
    def is_lob(self):
        return self.is_binary() or self.type == Column.Type.text
    
    def is_text(self):
        """ Returns True if the column is either of type string or text.   """
        return self.type == Column.Type.string or self.type == Column.Type.text
    
    def is_binary(self):
        return self.type == Column.Type.binary
        
    def is_number(self):
        """ Returns True if the column is either of type integer, float or decimal. """
        return self.type == Column.Type.int \
                or self.type == Column.Type.float \
                or self.type == Column.Type.decimal
    
    def is_time(self):
        return self.type == Column.Type.date \
                or self.type == Column.Type.datetime \
                or self.type == Column.Type.timestamp
    
    def is_boolean(self):
        return self.type == Column.Type.boolean
    
    def type_cast(self, value):
        """ Casts value (which is a String) to an appropriate instance. """
        if value is None: return None
        if   self.type == Column.Type.string:       return value.strip()
        elif self.type == Column.Type.text:         return value.strip()
        elif self.type == Column.Type.int:          return to_i(value)
        elif self.type == Column.Type.float:        return to_f(value)
        elif self.type == Column.Type.decimal:      return to_decimal(value)
        elif self.type == Column.Type.datetime:     return str2datetime(value)
        elif self.type == Column.Type.timestamp:    return str2datetime(value)
        elif self.type == Column.Type.date:         return str2date(value)
        elif self.type == Column.Type.binary:       return binary2str(value)
        elif self.type == Column.Type.boolean:      return to_int(to_bool(value))
        else: return value
    
    def db_field_type(self, column_type):
        if   column_type == Column.Type.string:     return 'VARCHAR' 
        elif column_type == Column.Type.text:       return 'TEXT' 
        elif column_type == Column.Type.int:        return 'INTEGER' 
        elif column_type == Column.Type.float:      return 'FLOAT' 
        elif column_type == Column.Type.decimal:    return 'DECIMAL' 
        elif column_type == Column.Type.date:       return 'DATE' 
        elif column_type == Column.Type.datetime:   return 'DATETIME' 
        elif column_type == Column.Type.timestamp:  return 'TIMESTAMP' 
        elif column_type == Column.Type.binary:     return 'BLOB' 
        elif column_type == Column.Type.boolean:    return 'TINYINT'
        else : raise Exception('Can not support %s column type !' % column_type)

    def to_sql(self):
        if not self.name:
            raise Exception('Name can not a empty value.')
         
        sql = '`%s` %s' % (self.name, self.db_field_type(self.type))
            
        if self.length:
            sql = '%s(%d)' % (sql, self.length)
            
        if self.precision:
            sql = '%s(%d' % (sql, self.precision)
            if self.scale:
                sql = '%s, %d' % (sql, self.scale)
            sql = '%s)' % sql
        
        if self.null:
            sql = '%s NULL' % sql 
        else:
            sql = '%s NOT NULL' % sql
        
        if self.default is None:
            pass
        elif self.is_number() or self.is_boolean():
            sql = "%s DEFAULT %d" % (sql, self.default)
        else:
            sql = "%s DEFAULT '%s'" % (sql, self.default) 
            
        return sql
    
    def __str__(self):
        l = []
        
        if self.name is not None:       l.append('name=%s' % self.name)
        if self.type is not None:       l.append('type=%s' % self.type)
        if self.length is not None:     l.append('length=%s' % self.length) 
        
        l.append('null=%s' % self.null)
        
        if self.default is not None:    l.append('default=%s' % self.default) 
        if self.precision is not None:  l.append('precision=%s' % self.precision)
        if self.scale is not None:      l.append('scale=%s' % self.scale)

        return 'Column[ %s ]' % ', '.join(l)

    
class Table(object):
    
    id_type = namedtuple('id_type', ['auto', 'uuid'])._make(['auto', 'uuid'])
    
    def __init__(self, name, id = id_type.auto):
        self.__name         = name.lower()
        self.columns        = []
        self.__id           = id or Table.id_type.auto
        self.__id_column    = self.__create__id_column()
        
    @property
    def name(self):
        return self.__name
        
    @property
    def id(self):
        return self.__id
    
    def is_auto_id(self):
        return self.__id == Table.id_type.auto
    
    def is_uuid_id(self):
        return self.__id == Table.id_type.uuid
             
    def __create__id_column(self):
        if self.is_auto_id(): 
            return Column('id', Column.Type.int)
            
        if self.is_uuid_id():
            return Column('id', Column.Type.string, length = 32)
            
    def contains(self, column):
        for c in self.columns:
            if c.name == column.name:
                return True
        return False
    
    def add_column(self, column):
        if self.contains(column):
            raise ColumnExistError('%s column is exist. can not add again.' % column.name)
        self.columns.append(column)

    def __id_column_to_sql(self):
        if self.is_auto_id():
            return '%s AUTO_INCREMENT' % self.__id_column.to_sql()
        if self.is_uuid_id():
            return '%s' % self.__id_column.to_sql()

    def to_sql(self):
        strbuff = []
        strbuff.append('CREATE TABLE IF NOT EXISTS `%s`(\n' % self.name)
        strbuff.append('\t%s,\n' % self.__id_column_to_sql())
        for column in self.columns:
            strbuff.append('\t%s,\n' % column.to_sql())
        strbuff.append('\tPRIMARY KEY(`id`)')
        strbuff.append(');')
        
        return ''.join(strbuff)
        
    def __setattr__(self, name, value):
        if name != '_Table__id_column' and isinstance(value, Column):
            value.name = name.lower().strip()
            self.columns.append(value)
        else:
            self.__dict__[name] = value

    def __str__(self):
        return 'Table[ %s ]' % self.name

    def __iter__(self):
        for c in self.columns:
            yield c
