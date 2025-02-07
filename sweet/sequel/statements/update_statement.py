from typing import Self

from sweet.sequel.terms.name import Name
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.value import Value1
from sweet.sequel.terms.where import Where


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
        self.where_clause = Where()
        self.sets : {str: Value1} = {}
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
            self.sets[k] = v
        return self

    def where(self, *qs: Q, **kwargs) -> Self:
        """
        Add filtering conditions for the UPDATE statement.

        :param qs: `Q` objects represents filter conditions.
        :param kwargs: keyword arguments for creating filter conditions. (e.g., `id=1`)
        :return: The current UpdateStatement instance
        """
        self.where_clause.append(*qs, **kwargs)
        return self

