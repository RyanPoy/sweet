from typing import List, Any, Self
from sweet.sequel.operator import opt
from sweet.utils import BasicType, is_hash, is_array


class Insert:

    def __init__(self, tablename: str, placeholder: str):
        self.tablename = f'"{tablename}"'
        self.placeholder = placeholder
        self.insert_list = []
        self.returning_columns = []

    def insert(self, records: list[dict] = None, **kwargs) -> Self:
        if records:
            if is_hash(records):
                self.insert_list.append(records)
            elif is_array(records):
                self.insert_list.extend(records)
        if kwargs:
            self.insert_list.append(kwargs)
        return self

    def returning(self, *columns: list[str]) -> Self:
        self.returning_columns.extend(columns)
        return self

    def sql(self) -> (str, list[BasicType]):
        if not self.insert_list:
            return "", []

        cols = self.insert_list[0].keys()
        if len(self.insert_list) > 1:
            for r in self.insert_list:
                if r.keys() != cols:
                    raise Exception("multiple insert only support same columns")

        values_sql, params = [], []
        for r in self.insert_list:
            pls = ', '.join([self.placeholder]*len(r))
            values_sql.append(f"({pls})")
            params.extend(r.values())

        cols_sql = ', '.join(map(lambda c: f'"{c}"', cols))
        returning_sql = '' if not self.returning_columns else ', '.join(map(lambda c: f'"{c}"', self.returning_columns))

        if not returning_sql:
            return f'INSERT INTO {self.tablename} ({cols_sql}) VALUES {', '.join(values_sql)}', params
        return f'INSERT INTO {self.tablename} ({cols_sql}) VALUES {', '.join(values_sql)} RETURNING ({returning_sql})', params
