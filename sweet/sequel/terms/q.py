from typing import List, Optional, Self

from sweet.sequel.terms import Logic
from sweet.sequel.terms.binary import Binary, parse
from sweet.sequel.terms.values import ValueType


class Q:
    def __init__(self, **kwargs: ValueType):
        self.binaries: List[Binary] = []
        for k, v in kwargs.items():
            self.binaries.append(parse(**{k: v}))

        self.operator: Optional[Logic] = None  # None 表示基础条件
        self.left: Optional[Q] = None
        self.right: Optional[Q] = None
        self.invert: bool = False

    def is_empty(self) -> bool:
        return not self.binaries and not self.right

    def __eq__(self, other):
        return (self.__class__ == other.__class__ and self.left == other.left and self.right == other.right
                and self.invert == other.invert and self.binaries == other.binaries)

    def __hash__(self):
        # return hash(f'{self.__class__}-{str(self.left)}-{str(self.right)}-{self.operator}-{"-".join([str(x) for x in self.binaries])}')
        return hash(f'{self.__class__}-{str(self.left)}-{str(self.right)}-{self.operator}-{"-".join([str(x) for x in self.binaries])}')

    def __invert__(self) -> Self:
        q = Q()
        q.binaries = self.binaries
        q.operator = self.operator
        q.left = self.left
        q.right = self.right
        q.invert = not self.invert
        return q

    def __and__(self, other) -> Self:
        return self.__combine(other, Logic.AND)

    def __or__(self, other) -> Self:
        return self.__combine(other, Logic.OR)

    def __combine(self, other: Self, logic_op: Logic) -> Self:
        if not isinstance(other, Q):
            raise TypeError("Logical operators can only be applied between two Q objects.")

        if not other.binaries and not other.right:
            return self
        if not self.binaries and not self.right:
            return other

        q = Q()
        q.operator = logic_op
        q.left = self
        q.right = other
        return q


# # 基础 NOT 条件
# q = ~Q(a=10)
# print(q.sql() == 'NOT a = 10')
#
# # 组合 NOT 条件
# q = ~Q(a=10) | ~Q(b=10)
# print(q.sql() == 'NOT a = 10 OR NOT b = 10')
#
# # 复杂嵌套条件
# q = ~(~Q(a=10) | ~Q(b=10)) & ~(Q(c=10) | ~Q(d=10))
# print(q.sql() == '(NOT (NOT a = 10 OR NOT b = 10)) AND (NOT (c = 10 OR NOT d = 10))')
#
#
# # 多层嵌套
# q = ~(Q(a=10) & Q(b=20)) | ~(Q(c=30) & Q(d=40))
# print(q.sql() == 'NOT (a = 10 AND b = 20) OR NOT (c = 30 AND d = 40)')
#
# # 基础条件
# q = Q(a=10, b=10)
# print(q.sql() == 'a = 10 AND b = 10')
#
# # 链式组合
# q = Q(a=10) | Q(b=10)
# print(q.sql() == 'a = 10 OR b = 10')
# q = Q(a=10) & Q(b=10)
# print(q.sql() == 'a = 10 AND b = 10')
#
# # 复杂嵌套（自动处理优先级和括号）
# q = Q(a=10) | Q(b=10) & (Q(c=10) | Q(d=10))
# print(q.sql() == 'a = 10 OR b = 10 AND (c = 10 OR d = 10)')
#
# q = (Q(a=10) | Q(b=10)) & Q(c=10) | Q(d=10)
# print(q.sql() == '(a = 10 OR b = 10) AND c = 10 OR d = 10')
#
# q = (Q(a=10) | Q(b=10)) & (Q(c=10) | Q(d=10))
# print(q.sql() == '(a = 10 OR b = 10) AND (c = 10 OR d = 10)')
#
# q = (Q(a=10) | Q(b=10)) & (Q(c=10) | Q(d=10)) & (Q(e=10) | Q(f=10))
# print(q.sql() == '(a = 10 OR b = 10) AND (c = 10 OR d = 10) AND (e = 10 OR f = 10)')
