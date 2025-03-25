from __future__ import annotations

from typing import Self, TYPE_CHECKING

from sweet.model.relation import Relation
from sweet.sequel.terms.name_fn import Count
from sweet.sequel.visitors.visitor import Visitor
from sweet.utils import classproperty
from sweet.model import consts

if TYPE_CHECKING:
    from sweet.model import Model

from sweet.sequel.statements.select_statement import SelectStatement
from sweet.sequel.terms.binary import Binary, Q


class Objects:

    def __init__(self, model_class: 'Model') -> None:
        self.model_class = model_class
        self.binary = None
        self._execute_func = None

    @classproperty
    def adapter(cls):
        return getattr(cls, consts.db_adapter)

    @classproperty
    def sql_visitor(cls) -> Visitor:
        return getattr(cls, consts.sql_visitor)()

    def filter(self, **kwargs) -> Self:
        binary = Q(**kwargs)
        if self.binary is not None:
            binary = self.binary & binary
        return self._copy_with(binary)

    def _copy_with(self, binary: Binary) -> Self:
        objs = self.__class__(self.model_class)
        objs.binary = binary
        return objs

    def all(self) -> Self:
        stmt = SelectStatement().from_(self.model_class.table.name_named).where(self.binary)
        return Relation(stmt, self.sql_visitor)

    def first(self) -> 'Model' | None:
        stmt = SelectStatement().from_(self.model_class.table.name_named).where(self.binary).limit(1)
        sql = self.sql_visitor.sql(stmt)
        return self.adapter.fetchone(sql)

    def last(self) -> 'Model' | None:
        stmt = SelectStatement().from_(self.model_class.table.name_named).where(self.binary)
        sql = self.sql_visitor.sql(stmt.select(Count()))
        cnt = self.adapter.fetchone(sql)
        if not cnt:
            return None

        sql = self.sql_visitor.sql(stmt.limit(1).offset(cnt))
        return self.adapter.fetchone(sql)
