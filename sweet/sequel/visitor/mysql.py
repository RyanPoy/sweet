from datetime import date, datetime
from decimal import Decimal

from sweet.sequel import quoting
from sweet.sequel.visitor.to_sql import ToSql
from sweet.utils import datetime2str, date2str, binary2str


class MySQL(ToSql):
    BIND_MARKER = '%s'

    def holder_lst(self, placeholder: str, length: int, begin: int) -> (str, int):
        """  holder_lst_qmark('?', 4, 3) -> ['?', '?', '?', '?'], 7 """
        return ', '.join([placeholder] * length), begin + length
