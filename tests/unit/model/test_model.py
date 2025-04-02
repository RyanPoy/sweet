from sweet.sequel.terms.name_fn import Name
from tests.helper import User


def test_table_name():
    assert "users" == User.table.name
    assert Name("users") == User.table.name_named
