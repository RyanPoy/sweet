from __future__ import annotations

import decimal
from abc import abstractmethod
from dataclasses import dataclass, field
from datetime import date, datetime
from decimal import Decimal
from time import time
from typing import Self

from sweet import utils
from sweet.sequel.terms.name_fn import Name


class Column:

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: any = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None) -> None:
        self.name = name
        self._value = None
        self.is_pk = is_pk
        self.is_null = is_null
        self.default = default
        self.unique = unique
        self.db_index = db_index
        self.description = description
        self.validators = validators or []

        self._check_after_init()

    def __set_name__(self, owner, name):
        if self.name is None:
            self.name = name

    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, value):
        if value is None:
            self._value = None
            return
        try:
            self._value = self._purify(value)
        except (ValueError, decimal.InvalidOperation, AttributeError) as _:
            raise ValueError(f"Can't purify {value}, it's a {value.__class__.__name__} type.")

    @abstractmethod
    def _purify(self, value):
        raise NotImplementedError

    def _check_after_init(self):
        pass


class CharColumn(Column):
    """
    mysql: varchar, char, text
    sqlite: text
    postgresql: varchar, char, text
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None, length: int = 64):
        self.length = length
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _check_after_init(self):
        err = None
        try:
            self.length = int(self.length)
        except Exception as ex:
            err = ex
        finally:
            if err or self.length <= 0:
                raise ValueError(f"The length must be greater than zero, but got {self.length}")

    def _purify(self, value):
        return str(value)


class TextColumn(CharColumn):
    """
    mysql: text, longtext, mediumtext
    sqlite: text
    postgresql: text
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None, length: int = 1024):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators, length=length)


class BinaryColumn(CharColumn):
    """
    mysql: blob, varbinary
    sqlite: blob
    postgresql: by
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None, length: int = 1024):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators, length=length)

    def _purify(self, value):
        return value.encode("UTF8")


class IntColumn(Column):
    """
    mysql: int, tinyint, smallint, mediumint, bigint
    sqlite: integer
    postgresql: integer, bigint, smallint
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: int = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.to_i(value)


class BooleanColumn(Column):
    """
    mysql: boolean, tinyint(1)
    sqlite: integer
    postgresql: boolean
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: bool = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.to_bool(value)


class FloatColumn(Column):
    """
    mysql: float, double, Decimal
    sqlite: real
    postgresql: real, double precision
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: float = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.to_f(value)


class DecimalColumn(Column):
    """
    mysql: decimal, numeric
    sqlite: real
    postgresql: decimal, numeric
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: Decimal = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.to_decimal(value)


class DateColumn(Column):
    """
    mysql: date (YYYY-MM-DD)
    sqlite: date
    postgresql: date
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: date | datetime | str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.str2date(str(value))


class DatetimeColumn(Column):
    """
    mysql: datetime, timestamp
    sqlite: datetime
    postgresql: timestamp(无时区), timestamptz(有时区)
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: date | datetime | str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.str2datetime(str(value))


class TimeColumn(Column):
    """
    mysql: time
    sqlite: time
    postgresql: time
    """

    def __init__(self, name: str = None, is_pk: bool = False, is_null: bool = False, default: time | date | datetime | str = None,
                 unique: bool = False, db_index: bool = False, description: str = None, validators: list = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)

    def _purify(self, value):
        return utils.str2time(str(value))


@dataclass
class Columns:
    data: dict[str, Column] = field(default_factory=dict)

    def add(self, field_name: str, col: Column) -> Self:
        if field_name in self.data:
            raise ValueError(f"'Table' object exits column {field_name}")
        self.data[field_name] = col
        return self

    def __getattr__(self, name):
        if name in self.data:
            return self.data[name]
        return super().__getattribute__(name)

    def get(self, name: str) -> Column:
        return self.columns.get(name, None)


@dataclass
class Table:
    def __init__(self, name: str) -> None:
        self.__name: str = name
        self.__name_named: Name = Name(self.__name)

        # self.pk: Column = None
        self.columns: Columns = Columns()

    @property
    def name(self):
        return self.__name

    @name.setter
    def name(self, value):
        self.__name = value
        self.__name_named = Name(self.name)

    @property
    def name_named(self):
        return self.__name_named
