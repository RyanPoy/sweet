import pytest

from tests.helper.models import User


@pytest.mark.asyncio
async def test_insert_one_returns_id_in_mysql(mysql_env):
    id = await User.objects.insert({'name': "lucy no.1"})
    assert id == 1

    id = await User.objects.insert({'id': 10, 'name': "lucy no.2"})
    assert id == 10


@pytest.mark.asyncio
async def test_insert_one_returns_id_in_sqlite(sqlite_env):
    id = await User.objects.insert({'name': "lucy no.1"})
    assert id == 1

    id = await User.objects.insert({'id': 10, 'name': "lucy no.2"})
    assert id == 10


@pytest.mark.asyncio
async def test_insert_one_returns_id_in_pg(pg_env):
    id = await User.objects.insert({'name': "lucy no.1"})
    assert id == 1
    # id = await User.objects.insert({'name': "lucy no.1"})
    # assert id == 2

    # id = await User.objects.insert({'id': 10, 'name': "lucy no.2"})
    # assert id == 10

# @pytest.mark.asyncio
# async def test_insert_and_find(self, mysql_env):
#     await User.objects.insert(
#         {'id': 1, 'name': "Lily"},
#         {'name': "Luly"},
#         {'id': 3, 'name': "Jim"},
#     )
#     us = await User.objects.all()
#     assert len(us) == 3
#
# @pytest.mark.asyncio
# async def test_mysql(self, mysql_env):
#     u1 = await User.objects.insert({'id': 1, 'name': "lily"})
#     u2 = await User.objects.first()
#     assert u2.id == 1
#     assert u2.name == "lily"
