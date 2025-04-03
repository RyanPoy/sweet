from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Self

from sweet.driver import Driver
from sweet.utils import extract_number, extract_numbers, to_bool


class ColumnKind(Enum):
    Unavailable = auto()
    # mysql: int, tinyint, smallint, mediumint, bigint
    # sqlite: integer
    # postgresql: integer, bigint, smallint
    Integer = auto()

    # mysql: varchar, char, text
    # sqlite: text
    # postgresql: varchar, char, text
    String = auto()

    # mysql: text, longtext, mediumtext
    # sqlite: text
    # postgresql: text
    Text = auto()

    # mysql: boolean, tinyint(1)
    # sqlite: integer
    # postgresql: boolean
    Boolean = auto()

    # mysql: float, double, Decimal
    # sqlite: real
    # postgresql: real, double precision
    Float = auto()

    # mysql: decimal, numeric
    # sqlite: real
    # postgresql: decimal, numeric
    Decimal = auto()

    # mysql: date (YYYY-MM-DD)
    # sqlite: date
    # postgresql: date
    Date = auto()

    # mysql: datetime, timestamp
    # sqlite: datetime
    # postgresql: timestamp(无时区), timestamptz(有时区)
    Datetime = auto()

    # mysql: time
    # sqlite: time
    # postgresql: time
    Time = auto()

    # mysql: blob, varbinary
    # sqlite: blob
    # postgresql: by
    Binary = auto()


@dataclass
class Column:
    name: str = field(init=False)
    kind: ColumnKind = field(init=False)
    limit: int = field(init=False)
    null: bool = field(init=False)
    default: str = field(init=False)
    key: str = field(init=False)
    extra: str = field(init=False)
    precision: int = field(init=False)
    scale: int = field(init=False)

    def __init__(self, **kwargs) -> None:
        self.name = kwargs.get('name', '')
        self.limit = kwargs.get('limit', 0)
        self.null = to_bool(kwargs.get('null', 'YES'))
        self.default = kwargs.get('default', '')
        self.key = kwargs.get('key', '')
        self.extra = kwargs.get('extra', '')
        self.precision = kwargs.get('precision', 0)
        self.scale = kwargs.get('scale', 0)
        self.kind = kwargs.get('kind', ColumnKind.Unavailable)

        self.__post_init__()

    def __post_init__(self) -> None:
        if isinstance(self.kind, ColumnKind):
            return

        kind = self.kind
        if isinstance(kind, str):
            kind = kind.lower()

        if kind in {'int', 'tinyint', 'smallint', 'mediumint', 'bigint', 'integer'}:
            self.kind = ColumnKind.Integer
        elif kind.startswith('tinyint'):
            self.kind = ColumnKind.Integer
            limit = extract_number(kind, None)
            if limit is not None:
                self.limit = limit
        elif kind.startswith('varchar'):
            self.kind = ColumnKind.String
            limit = extract_number(kind, None)
            if limit is not None:
                self.limit = limit
        elif kind.startswith('char'):
            self.kind = ColumnKind.String
            limit = extract_number(kind, None)
            if limit is not None:
                self.limit = limit
        elif kind in {'text', 'mediumtext', 'longtext'}:
            self.kind = ColumnKind.Text
        elif kind in {'float', 'double', 'real', 'double precision'}:
            self.kind = ColumnKind.Float
        elif kind.startswith('decimal'):
            self.kind = ColumnKind.Decimal
            precision, scale = extract_numbers(kind, (None, None))
            if not (precision is None and scale is None):
                self.precision = precision
                self.scale = scale
        elif kind == 'numeric':
            self.kind = ColumnKind.Decimal
        elif kind == 'date':
            self.kind = ColumnKind.Date
        elif kind in {'datetime', 'timestamp', 'timestamptz'}:
            self.kind = ColumnKind.Datetime
        elif kind.startswith('timestamptz') or kind.startswith('timestamp'):
            self.kind = ColumnKind.Datetime
        elif kind == 'time' or kind.startswith('time'):
            self.kind = ColumnKind.Time
        elif kind in {'blob', 'bytea', 'byte'}:
            self.kind = ColumnKind.Binary
            self.limit = 1024
        elif kind.startswith('varbinary'):
            self.kind = ColumnKind.Binary
            limit = extract_number(kind, None)
            if limit is not None:
                self.limit = limit
        elif kind == 'boolean':
            self.kind = ColumnKind.Boolean
        else:
            raise TypeError(f'Can not parse column kind: {kind}')


@dataclass
class Columns:
    data: list[Column] = field(default_factory=list)

    @classmethod
    async def of(cls, driver: Driver, table_name) -> Self:
        cs = cls()
        rows = await driver.columns(table_name)
        for i, r in enumerate(rows):
            cs.append(Column(
                name=r['name'], null=r['null'], key=r['key'], default=r['default'],
                extra=r['extra'], kind=r['kind']
            ))
        return cs

    def append(self, col: Column) -> Self:
        self.data.append(col)
        return self

    def find_by_name(self, name) -> Column | None:
        for c in self.data:
            if c.name == name:
                return c
        return None


@dataclass
class Table:
    name: str
    columns: Columns

