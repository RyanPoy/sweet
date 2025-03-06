from dataclasses import dataclass, field
from datetime import date, datetime
from decimal import Decimal
from time import time
from typing import Dict, List, Optional, Self, Set, Union


class Column:

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: any = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None) -> None:
        self.name = name
        self.is_pk = is_pk
        self.is_null = is_null
        self.default = default
        self.unique = unique
        self.db_index = db_index
        self.description = description
        self.validators = validators or []

        self._check_after_init()

    def _check_after_init(self):
        pass

    def __set_name__(self, owner, name):
        if self.name is None:
            self.name = name

class CharColumn(Column):
    """
    mysql: varchar, char, text
    sqlite: text
    postgresql: varchar, char, text
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None, length: int = 64):
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


class TextColumn(CharColumn):
    """
    mysql: text, longtext, mediumtext
    sqlite: text
    postgresql: text
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None, length: int = 1024):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators, length=length)


class BinaryColumn(CharColumn):
    """
    mysql: blob, varbinary
    sqlite: blob
    postgresql: by
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: str = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None, length: int = 1024):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators, length=length)


class IntColumn(Column):
    """
    mysql: int, tinyint, smallint, mediumint, bigint
    sqlite: integer
    postgresql: integer, bigint, smallint
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: int = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class BooleanColumn(Column):
    """
    mysql: boolean, tinyint(1)
    sqlite: integer
    postgresql: boolean
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: bool = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class FloatColumn(Column):
    """
    mysql: float, double, Decimal
    sqlite: real
    postgresql: real, double precision
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: float = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class DecimalColumn(Column):
    """
    mysql: decimal, numeric
    sqlite: real
    postgresql: decimal, numeric
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: Decimal = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class DateColumn(Column):
    """
    mysql: date (YYYY-MM-DD)
    sqlite: date
    postgresql: date
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: Union[date, datetime, str] = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class DatetimeColumn(Column):
    """
    mysql: datetime, timestamp
    sqlite: datetime
    postgresql: timestamp(无时区), timestamptz(有时区)
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: Union[date, datetime, str] = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class TimeColumn(Column):
    """
    mysql: time
    sqlite: time
    postgresql: time
    """

    def __init__(self, name: Optional[str] = None, is_pk: bool = False, is_null: bool = False, default: Union[time, date, datetime, str] = None,
                 unique: bool = False, db_index: bool = False, description: Optional[str] = None, validators: Optional[List] = None):
        super().__init__(name=name, is_pk=is_pk, is_null=is_null, default=default, unique=unique, db_index=db_index, description=description,
                         validators=validators)


class Table:
    def __init__(self, name):
        self.name = name
