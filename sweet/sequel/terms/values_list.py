from typing import Self

from sweet.sequel.terms.value import Value, Value1, Values


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

    def __init__(self, *args: Values) -> None:
        self.data: [Values] = []
        self.append(*args)

    def is_empty(self):
        """
        Check if the ValuesList contains any rows.

        :return: True if the ValuesList is empty, otherwise False
        """
        return False if self.data else True

    def append(self, *args: Values) -> Self:
        """
        Append the row to the ValuesList. All rows must have consistent column length as existing rows.

        :param values: A list of tuples where each tuple represents a row to be added.
        :raises ValueError: If the rows have inconsistent length.
        :return: The current instance of ValuesList.
        """
        args = [ x for x in args if not x.is_empty() ]
        self.check_values(*args)
        self.data.extend(args)
        return self

    def check_values(self, *args: Values) -> None:
        if not args:
            return
        len0 = len(args[0])
        for values in args:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")

        for values in self.data:
            if len0 != len(values):
                raise ValueError("Inconsistent row length")
