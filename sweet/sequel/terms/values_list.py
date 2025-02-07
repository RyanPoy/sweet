from typing import Self

from sweet.sequel.terms.value import Value1


class ValuesList:
    """
    Represents a collection of rows to be used in SQL statements, such as INSERT or UPDATE.
    Each row is a tuple of value, and all rows must have consistent column length.

    Usage:
        # initialize with a single row
        vs = ValuesList(1, "lily", 20)

        # add a single row
        vs.append([(2, "lucy", 32)])

        # add multiple rows
        vs.append([ (3, "jimy", 15), (4, "abc", 8) ])
    """
    def __init__(self, *vs: Value1) -> None:
        self.data = []
        if vs:
            self.data.append(vs)

    def is_empty(self):
        """
        Check if the ValuesList contains any rows.

        :return: True if the ValuesList is empty, otherwise False
        """
        return False if self.data else True

    def append(self, rows: [(Value1,)]) -> Self:
        """
        Append the row to the ValuesList. All rows must have consistent column length as existing rows.

        :param rows: A list of tuples where each tuple represents a row to be added.
        :raises ValueError: If the rows have inconsistent length.
        :return: The current instance of ValuesList.
        """
        if not rows:
            return self

        len0 = len(rows[0])
        for row in rows:
            if len0 != len(row):
                raise ValueError("Inconsistent row length")
        for row in rows:
            self.data.append(row)
        return self
