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
from pyrails.activerecord.validators import *
from functools import partial


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
    def add(cls, partail, on=None):
        cls.__check(on).__validate_partials.setdefault(on, []).append(partail)
        return cls

    @classmethod
    def __check(cls, on):
        if on not in ('save', 'create', 'update'):
            raise Exception('argument on value must be in "save", "create" or "update"')
        return cls


def validates_percense_of(attr_names, allow_blank=True, on='save', msg='can not blank.'):
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
