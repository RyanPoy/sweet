from sweet.sequel.visitor.to_sql import ToSql


class PostgreSQL(ToSql):
    BIND_MARKER = '$'

    def holder_lst(self, placeholder: str, length: int, begin: int) -> (str, int):
        """  holder_lst_qmark('?', 4, 3) -> ['$1', '$2', '$3', '$4'], 7 """
        next = begin + length
        lst = [f'{placeholder}{x}' for x in range(begin, next)]
        return ', '.join(lst), next
