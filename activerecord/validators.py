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
import re

class NoOptionError(Exception):
    def __init__(self, options=[]):
        self.msg = 'should be in [%s], but noting' % ', '.join(options)
        

class OptionNameError(Exception):
    def __init__(self, option, options = []):
        should_be = ', '.join(options)
        self.msg = 'should be in [%s], but %s' % (should_be, option)


class Validator(object):
    
    def _validate_options(self, **options):
        all_null = True
        for opt in options:
            if opt not in self._options:
                raise OptionNameError(opt, self._options)
            else:
                if options[opt] is not None:
                    all_null = False
        if all_null:
            raise NoOptionError(self._options)
        return True
    
    
class PresenceValidator(Validator):
    
    def validate(self, value, allow_blank=True):
        if value is None:
            return False
        
        if allow_blank:
            if isinstance(value, str): # 字符串要单独处理
                if not value.strip():
                    return True
            if not value:
                return True
        else:
            if isinstance(value, str): # 字符串要单独处理
                if not value.strip():
                    return False
            if (isinstance(value, str) or isinstance(value, list) 
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
                return False
            
        return True
    
    
class InclusionValidator(Validator):
    
    def validate(self, value, in_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
            
        if isinstance(value, str):
            value = value.strip()
            
        if isinstance(value, str) and not value:
            return allow_blank
        return value in in_values


class NumericalityValidator(Validator):
    _options = set([
        'greater_than', 'greater_than_or_equal_to', 'equal_to', 
        'less_than', 'less_than_or_equal_to', 'odd', 'even', 'allow_null'
    ])
    
    def validate(self, value, *args, **options):
        self._validate_options(**options)
        if value is None:
            allow_null = options.get('allow_null', False)
            if allow_null:
                return True
            else:
                return False
        if type(value) not in (int, long, float):
            try:
                if '.' in value:
                    float(value)
                else:
                    int(value)
            except ValueError, _:
                return False
        if options.get('greater_than', False) and value <= options['greater_than']: 
            return False
        if options.get('greater_than_or_equal_to', False) and value < options['greater_than_or_equal_to']:
            return False
        if options.get('equal_to', False) and value != options['equal_to']:
            return False
        if options.get('less_than', False) and value >= options['less_than']:
            return False
        if options.get('less_than_or_equal_to', False) and value > options['less_than_or_equal_to']:
            return False
        odd = options.get('odd', None)
        if odd in (True, False):
            if odd:
                if value % 2 == 0: return False
            else:
                if value % 2 != 0: return False
        even = options.get('even', None)
        if even in (True, False):
            if even:
                if value % 2 != 0: return False
            else:
                if value % 2 == 0: return False
        return True
        

class AcceptanceValidator(Validator):
    
    def validate(self, value, allow_null=True):
        if allow_null:
            return True
        return value is not None
    
    
class LengthValidator(Validator):
    _options = set([
        '_is', 'minimum', 'maximum', 
        'allow_null', 'allow_blank'
    ])
    
    def validate(self, value, **options):
        self._validate_options(**options)
        if value is None:
            return options.get('allow_null', True)
        
        if options.get('allow_blank', True):
            if isinstance(value, str):
                if value.strip() == '':
                    return True
            if not value:
                return True
        else:
            if isinstance(value, str) or isinstance(value, unicode):
                if not value.strip():
                    return False
            if (isinstance(value, str) or isinstance(value, list) or isinstance(value, unicode)
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
                return False
            
        if options.get('_is', False) and len(value) != options['_is']: 
            return False
        if options.get('minimum', False) and len(value) < options['minimum']:
            return False
        if options.get('maximum', False) and len(value) > options['maximum']:
            return False
        return True


class ConfirmationValidator(Validator):
    
    def validate(self, value, confirmation_value):
        return confirmation_value == value


class FormatValidator(Validator):
    
    def validate(self, value, _with=None, allow_null=True, allow_blank=True):
        if value is None:
            return allow_null
        
        if allow_blank:
            if isinstance(value, str) or isinstance(value, unicode):
                if not value.strip():
                    return True
            if (isinstance(value, str) or isinstance(value, list) or isinstance(value, unicode)
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
                return True
        
        return True if re.match(_with, value) else False

    
class ExclusionValidator(Validator):
    
    def validate(self, value, exclusion_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
            
        if isinstance(value, str):
            value = value.strip()
            
        if (isinstance(value, str) or isinstance(value, list) 
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
            return allow_blank
        
        return value not in exclusion_values

