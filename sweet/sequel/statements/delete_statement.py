from typing import Self

from sweet.sequel.collectors import SQLCollector
from sweet.sequel.statements import Statement


class DeleteStatement(Statement):
    """
    DeleteStatement
    ├── Target: TableName
    │   └── Name: "table_name"
    ├── Filters: ConditionList (optional)
    │   ├── Condition: (e.g., column1 > value1)
    │   ├── Condition: (e.g., column2 = value2)
    │   ├── Condition: column1 IN Subquery
    │   │       └── Source: SelectStatement
    │   │           ├── Target: TableName
    │   │           │   └── Name: "another_table"
    │   │           │── JoinCondition:
    │   │           │   └── Condition: t1.column1 = t2.column1
    │   │           ├── Columns: ColumnList
    │   │           │   └── ColumnName: "column1"
    │   │           └── Filters: ConditionList
    │   │               └── Condition: column2 = Literal('value2')
    │   └── LogicalOperator: AND / OR
    └── Source: SelectStatement (optional, e.g., DELETE with subquery)
        ├── Target: TableName
        │   └── Name: "source_table"
        ├── Columns: ColumnList (optional, if needed by WHERE or JOIN)
        │   ├── ColumnName: "column1"
        │   ├── ColumnName: "column2"
        │   └── ColumnName: "column3"
        └── Filters: (e.g., WHERE or JOIN conditions)
    """
    def __init__(self) -> None:
        self.table = None

    def from_(self, table: "Table") -> Self:
        self.table = table
        return self

    # def filters(self, *qs: Q, **kwargs):
    #     pass
