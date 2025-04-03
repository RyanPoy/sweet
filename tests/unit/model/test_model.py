from sweet.sequel.terms.name_fn import Name
from tests.helper import User


def test_table_name():
    assert User.table.name == "users"
    assert User.table.name_named == Name("users")
