from typing import Self

from sweet.sequel.statements import Statement
from sweet.sequel.terms.q import Q
from sweet.utils import DBDataType


class UpdateStatement(Statement):
    """
    UpdateStatement
    ├── Target: TableName
    │   └── Name: "table_name"
    ├── Set: AssignmentList
    │   ├── Assignment: ColumnName = Value
    │   │   ├── ColumnName: "column1"
    │   │   └── Value: Literal("value1")
    │   ├── Assignment: ColumnName = Value
    │   │   ├── ColumnName: "column2"
    │   │   └── Value: Literal("value2")
    │   └── Assignment: ColumnName = Value
    │       ├── ColumnName: "column3"
    │       └── Value: Literal("value3")
    ├── Filters: ConditionList (optional)
    │   ├── Condition: column1 = Literal(10)
    │   ├── Condition: column2 > Literal(20)
    │   └── Condition: column3 IS NOT NULL
    └── Returning: ColumnList (optional)
        ├── ColumnName: "column1"
        ├── ColumnName: "column2"
        └── ColumnName: "column3"
    """
    def __init__(self):
        self.table = None
        self.wheres = []
        self.sets : {str: DBDataType} = {}

    def update(self, table: "Table") -> Self:
        self.table = table
        return self

    def set(self, **kwargs) -> Self:
        for k, v in kwargs.items():
            self.sets[k] = v
        return self

    def where(self, *qs: Q, **kwargs) -> Self:
        for q in qs:
            self.wheres.append(q)
        if kwargs:
            self.wheres.append(Q(**kwargs))
        return self
