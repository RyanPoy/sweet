from typing import Optional

from sweet.sequel.terms.name import Name


class Statement:

    def __init__(self):
        self._table_name : Optional[Name] = None

    @property
    def table_name(self) -> Name:
        return self._table_name
