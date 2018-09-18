#coding: utf8
from sweet.utils import is_str, is_num, is_blank_str
from functools import partial
import re


############### validators ###############
class PresenceValidator(object):
    
    def validate(self, value, allow_blank=True):
        if value is None:
            return False
        if allow_blank:
            return True
        if is_str(value) and is_blank_str(value): # 字符串要单独处理
            return allow_blank
        if value != 0 and not value: # emplty list, tuple, set, dict, str, unicode:
            return False
        return True
    
    
class InclusionValidator(object):
    
    def validate(self, value, in_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
        if is_str(value) and is_blank_str(value):
            return allow_blank
        if value != 0 and not value: # empty list, tuple, set, dict, str, unicode
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
            except ValueError as _:
                return False
        if (greater_than and value <= greater_than) or \
            (greater_than_or_equal_to and value < greater_than_or_equal_to) or \
            (equal_to and value != equal_to) or \
            (less_than and value >= less_than) or \
            (less_than_or_equal_to and value > less_than_or_equal_to):
            return False
        
        if odd is True and value % 2 == 0:
            return False
        if odd is False and value % 2 != 0:
            return False

        if even is True and value % 2 != 0:
            return False
        if even is False and value % 2 == 0:
            return False

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
        if is_str(value) and is_blank_str(value):
            return allow_blank
        if value != 0 and not value: # empty list, tuple, set, dict, str, unicode
            return allow_blank
        if (_is and len(value) != _is) or \
            (minimum and len(value) < minimum) or \
            (maximum and len(value) > maximum):  
            return False
        return True


class ConfirmationValidator(object):
    
    def validate(self, value, confirmation_value):
        return confirmation_value == value


class FormatValidator(object):
    
    def validate(self, value, _with, allow_null=True, allow_blank=True):
        if value is None:
            return allow_null
        if is_str(value) and is_blank_str(value):
            return allow_blank
        if value != 0 and not value: # empty list, tuple, set, dict, str, unicode
            return allow_blank
        return True if re.match(_with, value) else False

    
class ExclusionValidator(object):
    
    def validate(self, value, exclusion_values=[], allow_null=False, allow_blank=False):
        if value is None:
            return allow_null
        if is_str(value) and is_blank_str(value):
            return allow_blank
        if value != 0 and not value: # empty list, tuple, set, dict, str, unicode
            return allow_blank
        return value not in exclusion_values



#########################################
############# validations ###############
#########################################
class Validates(object):

    # store the validate partial function
    # the format is :
    # {
    #     "save": [],      # should be call if record create or update
    #     "update": [],   # should be call if record udpate
    #     "create": [],     # should be call if record create
    # }
    __validate_partials = {}

    @classmethod
    def next(cls, on='save'):
        """
        @args on: should throw exception if on not in save, 'create', 'update'
        """
        cls.__check(on)
        while cls.__validate_partials.get(on):
            yield cls.__validate_partials.get(on).pop()

    @classmethod
    def add(cls, partail, on='save'):
        cls.__check(on)
        cls.__validate_partials.setdefault(on, []).append(partail)
        return cls

    @classmethod
    def __check(cls, on):
        if on not in ('save', 'create', 'update'):
            raise Exception('argument on value must be in "save", "create" or "update"')
        return cls


def validates(method_name, on='save'):

    def _(record, method_name):
        return getattr(record, method_name)()

    p = partial(_, method_name=method_name)
    Validates.add(p, on)


def validates_of(attr_names, presence={}, uniqueness={}, format={}, length={}, 
                    inclusion={}, exclusion={}, numericality={}, confirmation={}):
    if presence:        validates_presence_of(attr_names, **presence)
    if uniqueness:      validates_uniqueness_of(attr_names, **uniqueness)
    if format:          validates_format_of(attr_names, **format)
    if length:          validates_length_of(attr_names, **length)
    if inclusion:       validates_inclusion_of(attr_names, **inclusion)
    if exclusion:       validates_exclusion_of(attr_names, **inclusion)
    if numericality:    validates_numericality_of(attr_names, **numericality)
    if confirmation:    validates_confirmation_of(attr_names, **confirmation)
    

def validates_presence_of(attr_names, allow_blank=True, on='save', msg='can not blank.'):
    p = partial(_validates_of, validator=PresenceValidator(), validate_attrs=_merge(attr_names), msg=msg, allow_blank=allow_blank)
    Validates.add(p, on)


def validates_uniqueness_of(attr_names, on='save', msg='has already been taken'):
    def _(record, attr_names, msg):
        validate = True
        validate_attrs = _merge(attr_names)
        for attr_name in validate_attrs:
            if record.__class__.where(**{attr_name: getattr(record, attr_name)}).count():
                record.add_error(attr_name, msg)
                validate = False
        return validate
    p = partial(_, attr_names=attr_names, msg=msg)
    Validates.add(p, on)


def validates_format_of(attr_names, _with, allow_null=False, allow_blank=False, on='save', msg='format error.'):
    p = partial(_validates_of, validator=FormatValidator(), validate_attrs=_merge(attr_names), msg=msg, _with=_with, 
                            allow_null=allow_null, allow_blank=allow_blank)
    Validates.add(p, on)


def validates_length_of(attr_names, _is=None, minimum=None, maximum=None, allow_null=False, allow_blank=False, 
                        on='save', msg='length unavailable error'):
    p = partial(_validates_of, validator=LengthValidator(), validate_attrs=_merge(attr_names), msg=msg, _is=_is, minimum=minimum, maximum=maximum, 
                            allow_null=allow_null, allow_blank=allow_blank)
    Validates.add(p, on)

    
def validates_inclusion_of(attr_names, in_values, allow_null=False, allow_blank=False, on='save', msg='inclusion error'):
    p = partial(_validates_of, validator=InclusionValidator(), validate_attrs=_merge(attr_names), msg=msg, in_values=in_values, 
                            allow_null=allow_null, allow_blank=allow_blank)
    Validates.add(p, on)


def validates_exclusion_of(attr_names, exclusion_values, allow_null=False, allow_blank=False, on='save', msg='inclusion error'):
    p = partial(_validates_of, validator=ExclusionValidator(), validate_attrs=_merge(attr_names), msg=msg, exclusion_values=exclusion_values, 
                            allow_null=allow_null, allow_blank=allow_blank)
    Validates.add(p, on)


def validates_numericality_of(attr_names, greater_than=None, greater_than_or_equal_to=None, equal_to=None, less_than=None, 
                                less_than_or_equal_to=None, odd=None, even=None, on='save', msg="numericality error"):
    p = partial(_validates_of, validator=NumericalityValidator(), validate_attrs=_merge(attr_names), msg=msg, greater_than=greater_than, 
                            greater_than_or_equal_to=greater_than_or_equal_to, equal_to=equal_to, less_than=less_than, 
                            less_than_or_equal_to=less_than_or_equal_to, odd=odd, even=even)
    Validates.add(p, on)


def validates_confirmation_of(attr_names, on='save', msg='confirmation error'):
    def _(record, attr_names, msg):
        validate = True
        validate_attrs = _merge(attr_names)
        validator = ConfirmationValidator()
        for attr_name in validate_attrs:
            confirm_attr_value = getattr(record, attr_name, None)
            attr_value = getattr(record, attr_name.split('confirm_')[-1], None)
            if not validator.validate(confirm_attr_value, attr_value):
                record.add_error(attr_name, msg)
                validate = False
        return validate
    p = partial(_, attr_names=attr_names, msg=msg)
    Validates.add(p, on)


def _validates_of(record, validator, validate_attrs, msg, *args, **kwargs):
    validate = True
    for attr_name in validate_attrs:
        if not validator.validate(getattr(record, attr_name), *args, **kwargs):
            record.add_error(attr_name, msg)
            validate = False
    return validate


def _merge(attr_names):
    if isinstance(attr_names, str):
        attr_names = [attr_names]
    return [ attr_name for attr_name in attr_names if attr_name and attr_name.strip() ]
