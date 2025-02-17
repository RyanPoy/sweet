from typing import Self

from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.terms.filter import Filter


class DeleteStatement:
    """
    Represents the Abstract Syntax Tree (AST) for a SQL DELETE statement.

    The DeleteStatement AST is structured as follows:

    DeleteStatement
    ├── Target: Name
    ├── Filters: ConditionList (optional)
    │   ├── Condition: (e.g., column1 > value1)
    │   ├── Condition: (e.g., column2 = value2)
    │   ├── Condition: column1 IN Subquery
    │   │       └── Source: SelectStatement
    │   │           ├── Target: Name
    │   │           │   └── Name: "another_table"
    │   │           │── JoinCondition:
    │   │           │   └── Condition: t1.column1 = t2.column1
    │   │           ├── Columns: ColumnList
    │   │           │   └── ColumnName: "column1"
    │   │           └── Filters: ConditionList
    │   │               └── Condition: column2 = Literal('value2')
    │   └── LogicalOperator: AND / OR
    └── Source: SelectStatement (optional, e.g., DELETE with subquery)
        ├── Target: Name
        │   └── Name: "source_table"
        ├── Columns: ColumnList (optional, if needed by WHERE or JOIN)
        │   ├── ColumnName: "column1"
        │   ├── ColumnName: "column2"
        │   └── ColumnName: "column3"
        └── Filters: (e.g., WHERE or JOIN conditions)

    Usage:
        # Create a Delete statement and specify the target table
        stmt = DeleteStatement(Name("users"))

        # add filter conditions
        stmt.where(id__lt=10).where(Q(name__like="%abc") | Q(name="lucy"))

        # generator the sql of database (e.g. MySQL)
        MySQLVisitor().sql(stmt)
    """
    def __init__(self, table_name: Name) -> None:
        self.where_clause : Filter = Filter()
        self.table_name : Name = table_name

    def where(self, *bs: Binary, **kwargs) -> Self:
        self.where_clause.add(*bs, **kwargs)
        return self
