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
from pyrails.activesupport import is_str, is_num, is_blank_str
import re

    
class PresenceValidator(object):
    
    def validate(self, value, allow_blank=True):
        if value is None:
            return False
        
        if allow_blank:
            return True
        
        if is_str(value): # 字符串要单独处理
            value = value.strip()
        if value != 0 and not value: # emplty list, tuple, set, dict, str, unicode:
            return False

        return True
    
    
class InclusionValidator(object):
    
    def validate(self, value, in_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
            
        if is_str(value) and is_blank_str(value):
            return allow_blank

        return value in in_values


class NumericalityValidator(object):
    
    def validate(self, value, allow_null=False, greater_than=None, greater_than_or_equal_to=None, 
                    equal_to=None, less_than=None, less_than_or_equal_to=None, odd=None, even=None):
        if value is None:
            return allow_null

        if not is_num(value):
            try:
                value = float(value) if '.' in value else int(value)
            except ValueError, _:
                return False
        if (greater_than and value <= greater_than) or \
            (greater_than_or_equal_to and value < greater_than_or_equal_to) or \
            (equal_to and value != equal_to) or \
            (less_than and value >= less_than) or \
            (less_than_or_equal_to and value > less_than_or_equal_to):
            return False
        
        if odd in (True, False):
            if odd:
                if value % 2 == 0: return False
            else:
                if value % 2 != 0: return False

        if even in (True, False):
            if even:
                if value % 2 != 0: return False
            else:
                if value % 2 == 0: return False
        return True
        

class AcceptanceValidator(object):
    
    def validate(self, value, allow_null=True):
        if allow_null:
            return True
        return value is not None
    
    
class LengthValidator(object):
    
    def validate(self, value, allow_null=True, allow_blank=True, _is=None, minimum=None, maximum=None):
        if value is None:
            return allow_null
        
        if allow_blank:
            if is_str(value) and is_blank_str(value):
                return True

            if not value:
                return True
        else:
            if is_str(value):
                if not value.strip():
                    return False
            if (is_str(value) or isinstance(value, list) 
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
                return False
            
        if (_is and len(value) != _is) or \
            (minimum and len(value) < minimum) or \
            (maximum and len(value) > maximum):  
            return False
        return True


class ConfirmationValidator(object):
    
    def validate(self, value, confirmation_value):
        return confirmation_value == value


class FormatValidator(object):
    
    def validate(self, value, _with=None, allow_null=True, allow_blank=True):
        if value is None:
            return allow_null
        
        if allow_blank:
            if is_str(value) or isinstance(value, unicode):
                if not value.strip():
                    return True
            if (is_str(value) or isinstance(value, list) or isinstance(value, unicode)
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
                return True
        
        return True if re.match(_with, value) else False

    
class ExclusionValidator(object):
    
    def validate(self, value, exclusion_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
            
        if is_str(value):
            value = value.strip()
            
        if (is_str(value) or isinstance(value, list) 
                or isinstance(value, dict) or isinstance(value, tuple)) and not value: # 数字0不用做这个判断
            return allow_blank
        
        return value not in exclusion_values

