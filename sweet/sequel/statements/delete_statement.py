from typing import Self

from sweet.sequel.terms.name import Name
from sweet.sequel.terms.q import Q
from sweet.sequel.terms.where import Where


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
        self.where_clause : Where = Where()
        self.table_name : Name = table_name

    def where(self, *qs: Q, **kwargs) -> Self:
        """
        Set the WHERE filter for the SQL statement.

        This method allows you to add one or more conditions to the WHERE clause of the SQL query.
        You can use it to chain multiple conditions, including combining them with logical operators like `|` (OR)、 `&` (AND).

        :param qs: One or more `Q` objects representing conditions for the WHERE clause.
                   You can combine multiple conditions using logical operators (e.g., `|` for OR; `&` for AND).
        :param kwargs: Keyword arguments that will be converted into a `Q` object. Each key is the field name,
                       and the value is the filter condition (e.g., `id=1`；`name__like="%_joke"`).
        :return: The current instance of the statement

        :examples:
            # Single condition
            stmt.where(id=1)

            # Use a like condition
            stmt.where(name__like="%_joke")

            # Combining multiple conditions with logic operator
            stmt.where(Q(age__gt=40) | Q(age__lt=20))

            # Chaining multiple where conditions
            stmt.where(Q(age__gt=40)).where(Q(age__lt=20))
        """
        self.where_clause.append(*qs, **kwargs)
        return self
