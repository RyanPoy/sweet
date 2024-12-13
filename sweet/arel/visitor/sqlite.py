from sweet.arel import SqlLiteral
from sweet.arel.visitor.to_sql import ToSql
from sweet.utils import replace_multiple


class SQLite(ToSql):
    BIND_MARKER = '?'
    QUOTED_COLUMN_NAMES: dict[str, str] = { }
    QUOTED_TABLE_NAMES: dict[str, str] = { }

    def holder_lst(self, placeholder: str, length: int, begin: int) -> (str, int):
        """  holder_lst_qmark('?', 4, 3) -> ['?', '?', '?', '?'], 7 """
        return ', '.join([placeholder] * length), begin + length

    def quote_column_name(self, name: str | SqlLiteral) -> str:
        if isinstance(name, SqlLiteral): return name.value
        return self.QUOTED_COLUMN_NAMES.get(name, replace_multiple(name, [ ('"', '""') ]))

    def quote_table_name(self, name: str) -> str:
        if isinstance(name, SqlLiteral): return name.value
        return self.QUOTED_TABLE_NAMES.get(name, replace_multiple(name, [ ('"', '""'), (".", '"."') ]))
