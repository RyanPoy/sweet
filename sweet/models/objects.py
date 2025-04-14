from __future__ import annotations
import copy
from typing import Self, TYPE_CHECKING

from sweet.db.drivers import MySQLDriver, PostgreSQLDriver, SQLiteDriver
from sweet.sequel.statements.insert_statement import InsertStatement
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.visitors.visitor import Visitor
from sweet.utils import class_property
from sweet.models.columns import Column
from sweet import consts

if TYPE_CHECKING:
    from sweet.models import Model

from sweet.sequel.statements.select_statement import SelectStatement


class Objects:

    def __init__(self, model_class: Model) -> None:
        self.model_class: Model = model_class
        self.__select_stmt = None

    @property
    def _select_stmt(self):
        if self.__select_stmt is None:
            self.__select_stmt = SelectStatement().from_(self.model_class.table.name_named)
        return self.__select_stmt

    @_select_stmt.setter
    def _select_stmt(self, value: SelectStatement) -> None:
        self.__select_stmt = value

    # noinspection PyMethodParameters
    @class_property
    def adapter(cls):
        return getattr(cls, consts.db_adapter)

    # noinspection PyMethodParameters
    @class_property
    def sql_visitor(cls) -> Visitor:
        return getattr(cls, consts.sql_visitor)()

    def filter(self, **kwargs) -> Self:
        """
        @todo: key-value对中的key，可能是不存在的column，这样就有问题了，需要进行校验
        :param kwargs:
        :return:
        """
        self.check_column_exists(kwargs)
        objs = self._copy()
        objs._select_stmt.where(**kwargs)
        return objs

    def all(self) -> Self:
        sql = self.sql()
        return self.adapter.fetchall(sql)

    async def first(self) -> Model | None:
        stmt = copy.deepcopy(self._select_stmt).limit(1)
        sql = self.sql_visitor.sql(stmt)
        d = await self.adapter.fetchone(sql)
        return self.model_class(**d)

    # def last(self) -> 'Model' | None:
    #     stmt = SelectStatement().from_(self.model_class.table.name_named).where(self.binary)
    #     sql = self.sql_visitor.sql(stmt.select(Count()))
    #     cnt = self.adapter.fetchone(sql)
    #     if not cnt:
    #         return None
    #
    #     sql = self.sql_visitor.sql(stmt.limit(1).offset(cnt))
    #     return self.adapter.fetchone(sql)

    def sql(self):
        return self.sql_visitor.sql(self._select_stmt)

    def _copy(self):
        objs = self.__class__(self.model_class)
        objs.__select_stmt = copy.deepcopy(self.__select_stmt)
        return objs

    async def insert(self, *records: [dict]) -> None:
        if len(records) > 1:
            return await self.insert_many(records)
        return await self.insert_one(records[0])

    async def insert_one(self, record: dict):
        self.check_column_exists(record)
        cols, values, id_manualed = [], [], False
        pk_names = set(self.model_class.table.pk_names)
        for k, v in record.items():
            if k in pk_names:
                id_manualed = True
            cols.append(Name(k))
            values.append(v)
        stmt = InsertStatement(self.model_class.table.name_named)
        stmt.column(*cols).insert(values)

        adapter_class = type(self.adapter)
        if adapter_class is MySQLDriver:
            sql = self.sql_visitor.sql(stmt)
            id = await self.adapter.execute_lastrowid(sql)
        elif adapter_class is SQLiteDriver:
            if id_manualed:
                stmt.returning(*[Name(n) for n in self.model_class.table.pk_names])
                sql = self.sql_visitor.sql(stmt)
                id = await self.adapter.execute_returnrowid(sql)
            else:
                sql = self.sql_visitor.sql(stmt)
                id = await self.adapter.execute_lastrowid(sql)
        elif adapter_class is PostgreSQLDriver:
            stmt.returning(*[Name(n) for n in self.model_class.table.pk_names])
            sql = self.sql_visitor.sql(stmt)
            id = await self.adapter.execute_returnrowid(sql)
        else:
            raise TypeError(f"Unknown driver type, expect MySQLDriver、SQLiteDriver、PostgresSQLDriver，but got {adapter_class}")
        return id

    async def insert_many(self, records: [dict]):
        pass
        # for r in records:
        #     self.check_column_exists(r)
        #
        # cols = [Name(k) for k, v in records[0].items()]
        # stmt = InsertStatement(self.model_class.table.name_named).column(*cols)
        # values = [tuple(r.values()) for r in records]
        # stmt.insert(*values)
        #
        # sql = self.sql_visitor.sql(stmt)
        # await self.adapter.execute(sql)

    def check_column_exists(self, r: dict) -> bool:
        for k, v in r.items():
            if not self.model_class.table.has_column(k):
                raise Column.DoesNotExist(k, self.model_class.table.name)
        return True
