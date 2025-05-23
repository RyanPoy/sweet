import pytest

from sweet.models import Column, Model
from sweet.models.objects import Objects
from tests.helper.models import User


def test_init_from_model():
    class Demo(Model): pass

    assert isinstance(Demo.objects, Objects)
    assert Demo.objects.model_class is Demo


class TestFilter:
    objs1 = User.objects.filter(id=10)
    objs2 = objs1.filter(name="username")

    def test_mysql(self, mysql_env):
        assert self.objs1._select_stmt != self.objs2._select_stmt
        assert self.objs1.sql() != self.objs2.sql()

    def test_sqlite(self, sqlite_env):
        assert self.objs1._select_stmt != self.objs2._select_stmt
        assert self.objs1.sql() != self.objs2.sql()

    def test_pg(self, sqlite_env):
        assert self.objs1._select_stmt != self.objs2._select_stmt
        assert self.objs1.sql() != self.objs2.sql()

    def test_filter__error_if_column_is_not_exists(self):
        with pytest.raises(Column.DoesNotExist, match=f"The column 'not_exist_column_name' does not exist in '{User.table.name}' table"):
            User.objects.filter(not_exist_column_name="whatever")


class TestAllSQL:
    objs = User.objects.filter(id=10).filter(name="username")

    @pytest.mark.asyncio
    async def test_mysql(self, mysql_env):
        assert self.objs.sql() == """SELECT * FROM `users` WHERE `id` = 10 AND `name` = 'username'"""

    @pytest.mark.asyncio
    async def test_sqlite(self, sqlite_env):
        assert self.objs.sql() == """SELECT * FROM "users" WHERE "id" = 10 AND "name" = 'username'"""

    @pytest.mark.asyncio
    async def test_pg(self, pg_env):
        assert self.objs.sql() == """SELECT * FROM "users" WHERE "id" = 10 AND "name" = 'username'"""

