from enum import Enum


class Logic(Enum):
    AND = 'AND'
    OR = 'OR'

    def __str__(self):
        return self.value

    def priority(self):
        """定义运算符优先级：and > or"""
        return 2 if self == Logic.AND else 1
