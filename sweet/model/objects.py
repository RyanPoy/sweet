from __future__ import annotations

import copy
from typing import Self, TYPE_CHECKING

from sweet.sequel.terms.name_fn import Count
from sweet.sequel.visitors.visitor import Visitor
from sweet.utils import class_property
from sweet.model import Column, consts

if TYPE_CHECKING:
    from sweet.model import Model

from sweet.sequel.statements.select_statement import SelectStatement


class Objects:

    def __init__(self, model_class: 'Model') -> None:
        self.model_class = model_class
        self.__select_stmt = None

    @property
    def _select_stmt(self):
        if self.__select_stmt is None:
            self.__select_stmt = SelectStatement().from_(self.model_class.table.name_named)
        return self.__select_stmt

    @_select_stmt.setter
    def _select_stmt(self, value: SelectStatement) -> None:
        self.__select_stmt = value

    @class_property
    def adapter(cls):
        return getattr(cls, consts.db_adapter)

    @class_property
    def sql_visitor(cls) -> Visitor:
        return getattr(cls, consts.sql_visitor)()

    def filter(self, **kwargs) -> Self:
        """
        @todo: key-value对中的key，可能是不存在的column，这样就有问题了，需要进行校验
        :param kwargs:
        :return:
        """
        for k, v in kwargs.items():
            if not self.model_class.table.has_column(k):
                raise Column.DoesNotExist(k, self.model_class.table.name)
        objs = self._copy()
        objs._select_stmt.where(**kwargs)
        return objs

    def all(self) -> Self:
        sql = self.sql()
        return self.adapter.fetchall(sql)

    # def first(self) -> 'Model' | None:
    #     stmt = SelectStatement().from_(self.model_class.table.name_named).where(self.binary).limit(1)
    #     sql = self.sql_visitor.sql(stmt)
    #     return self.adapter.fetchone(sql)
    #
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
