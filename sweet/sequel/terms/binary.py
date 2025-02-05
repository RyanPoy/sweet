from dataclasses import dataclass
from sweet.sequel.terms.name import Name
from sweet.utils import DBDataType


@dataclass
class Binary:
    key: Name
    value: Name | DBDataType = None
    op: Name | str = None

    def belongs_to_between(self):
        return self.op == 'BETWEEN' or self.op == 'NOT BETWEEN'


def Equal(key: Name, value: Name | str) -> Binary: return Binary(key, value, "=")


def NotEqual(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "<>")


def Is(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "IS")


def IsNot(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "IS NOT")


def In(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "IN")


def NotIn(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "NOT IN")


def Like(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "LIKE")


def NotLike(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "NOT LIKE")


def GreatThan(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, ">")


def GreatThanAndEqual(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, ">=")


def LessThan(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "<")


def LessThanAndEqual(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "<=")


def Between(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "BETWEEN")


def NotBetween(key: Name, value: Name | DBDataType) -> Binary: return Binary(key, value, "NOT BETWEEN")
