from sweet.sequel.visitor.to_sql import ToSql


class SQLite(ToSql):
    BIND_MARKER = '?'

    def holder_lst(self, placeholder: str, length: int, begin: int) -> (str, int):
        """  holder_lst_qmark('?', 4, 3) -> ['?', '?', '?', '?'], 7 """
        return ', '.join([placeholder] * length), begin + length
