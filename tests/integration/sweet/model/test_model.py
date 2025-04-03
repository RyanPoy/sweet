import pytest

from tests.helper import User, db


@pytest.mark.asyncio
async def test_insert_and_find(envs):

    for env in envs:
        async with db.using(env):
            User.objects.create(User(id=1, name="Lily"))
            User.objects.create(User(name="Luly"))
            User.objects.create(User(id=3, name="Jim"))

    for env in envs:
        async with db.using(env):
            us = User.objects.all()
            assert len(us) == 3
