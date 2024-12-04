from typing import Iterator


class Sequel:
    def __init__(self, placeholder: str):
        self.placeholder = placeholder

    def qoute_s(self, s: str) -> str:
        return f'"{s}"'

    def quote_lst(self, lst: list[str]) -> Iterator[str]:
        return map(lambda s: f'"{s}"', lst) # return map(lambda s: self.qoute_s(s), lst)

    def quote_lst_s(self, lst: list[str]) -> str:
        return ', '.join(self.quote_lst(lst))

    def quote_lst_parens_s(self, lst: list[str]) -> str:
        s = self.quote_lst_s(lst)
        return f'({s})'

    def holder_s(self, length: int) -> str:
        return ', '.join(self.holder_lst(length))

    def holder_parens_s(self, length: int):
        s = self.holder_s(length)
        return f'({s})'

    def holder_lst(self, length: int) -> list[str]:
        return [self.placeholder] * length


mysql = Sequel('%s')
sqlite = Sequel('?')
pg = Sequel(':')

# PosgressSequel
