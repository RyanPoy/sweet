from typing import Self

from sweet.sequel import Operator
from sweet.sequel.terms.binary import Binary
from sweet.sequel.terms.name_fn import Name
from sweet.sequel.terms.filter import Filter


class UpdateStatement:
    """
    Represents the Abstract Syntax Tree(AST) for a SQL UPDATE statement.

    The UpdateStatement is structured as follows:

    UpdateStatement
    ├── Target: Name
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

    Usage:
        # create an update statement
        stmt = UpdateStatement(Name("users"))

        # set column-value pairs for update
        stmt.set(name="lucy", age=20)

        # add filtering condition
        stmt.where(name="lily").where(Q(gender='m'))

        # generate SQL (e.g., for MySQL)
        MySQLVisitor().sql(stmt)
    """
    def __init__(self, table_name: Name):
        self.where_clause: Filter = Filter()
        self.sets : list[Binary] = []
        self.table_name: Name = table_name

    def set(self, **kwargs) -> Self:
        """
        Set the Column-Value pairs for the UPDATE statement
        :param kwargs: Column-Value pair.
        :return: The current UpdateStatement instance.

        :examples:
            stmt.set(name="lucy", age=20)
            stmt.set(is_active=True)
        """
        for k, v in kwargs.items():
            b = Binary.parse(**{k: v})
            b.op = Operator.EQ
            self.sets.append(b)
        return self

    def where(self, *bs: Binary, **kwargs) -> Self:
        self.where_clause.add(*bs, **kwargs)
        return self

