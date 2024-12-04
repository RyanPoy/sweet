qs          = lambda s: f'"{s}"'                                # 'a'  =>  '"a"'
qlist       = lambda lst: ', '.join(map(lambda s: qs(s), lst))  # ['a', 'b', 'c']  =>  '"a", "b", "c"'
qlist_parens = lambda lst, begin=1: f'({qlist(lst)})'            # ['a', 'b', 'c']  =>  '("a", "b", "c")'


def holder_lst_qmark(placeholder: str, length: int, begin: int):
    """  holder_lst_qmark('?', 4, 3) -> ['?', '?', '?', '?'], 7 """
    return ', '.join([placeholder] * length), begin + length


def holder_lst_numeric(placeholder: str, length: int, begin: int):
    """  holder_lst_qmark('?', 4, 3) -> ['$1', '$2', '$3', '$4'], 7 """
    next = begin + length
    lst = [f'{placeholder}{x}' for x in range(begin, next)]
    return ', '.join(lst), next


class Sequel:

    def __init__(self, placeholder, hlst_func):
        self.placeholder = placeholder
        self.hlst_func = hlst_func

    def holder_parens_s(self, length: int, begin: int) -> (str, int):
        """ holder_parens_s(5, 3) -> '(?, ?, ?, ?, ?)'
        """
        s, n = self.hlst_func(self.placeholder, length, begin)
        return f'({s})', n


mysql   = Sequel('%s', holder_lst_qmark)
sqlite  = Sequel('?', holder_lst_qmark)
pg      = Sequel('$', holder_lst_numeric)
