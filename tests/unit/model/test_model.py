from sweet.sequel.terms.name_fn import Name
from tests.helper.models import User


def test_table_name():
    assert User.table.name == "users"
    assert User.table.name_named == Name("users")
    assert User.column_names == ["id", "name"]
    assert len(User.table.pks) == 1
    assert User.table.pks[0].name == "id"