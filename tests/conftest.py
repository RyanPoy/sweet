from contextlib import asynccontextmanager

import pytest
import pytest_asyncio

from sweet.environment import Environment
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor
from sweet.utils import ObjDict
from tests.helper import sqls_mysql, sqls_postgresql, settings_mysql, settings_postgresql, settings_sqlite, sqls_sqlite


@pytest.fixture
def visitors():
    return ObjDict(
        mysql=MySQLVisitor(),
        sqlite=SQLiteVisitor(),
        pg=PostgreSQLVisitor(),
    )


@asynccontextmanager
async def _env(settings, create_sqls, drop_sqls):
    env = Environment(settings)
    try:
        await env.init_db()
        for sql in create_sqls:
            conn = await env.db.get_connection()
            await conn.execute(sql)

        yield env

        for sql in drop_sqls:
            conn = await env.db.get_connection()
            await conn.execute(sql)
    finally:
        await env.release_db()


@pytest_asyncio.fixture
async def mysql_env():
    async with _env(settings_mysql, sqls_mysql.CREATE_SQLS, sqls_mysql.DROP_SQLS) as env:
        yield env


@pytest_asyncio.fixture
async def sqlite_env():
    async with _env(settings_sqlite, sqls_sqlite.CREATE_SQLS, sqls_sqlite.DROP_SQLS) as env:
        yield env


@pytest_asyncio.fixture
async def pg_env():
    async with _env(settings_postgresql, sqls_postgresql.CREATE_SQLS, sqls_postgresql.DROP_SQLS) as env:
        yield env


@pytest.fixture
def singular_and_plural():
    return [
        ("search", "searches"),
        ("switch", "switches"),
        ("fix", "fixes"),
        ("box", "boxes"),
        ("process", "processes"),
        ("address", "addresses"),
        ("case", "cases"),
        ("stack", "stacks"),
        ("wish", "wishes"),
        ("fish", "fish"),

        ("category", "categories"),
        ("query", "queries"),
        ("ability", "abilities"),
        ("agency", "agencies"),
        ("movie", "movies"),

        ("archive", "archives"),

        ("index", "indices"),

        ("wife", "wives"),
        ("safe", "saves"),
        ("half", "halves"),

        ("move", "moves"),

        ("salesperson", "salespeople"),
        ("person", "people"),

        ("spokesman", "spokesmen"),
        ("man", "men"),
        ("woman", "women"),

        ("basis", "bases"),
        ("diagnosis", "diagnoses"),

        ("datum", "data"),
        ("medium", "media"),
        ("analysis", "analyses"),

        ("node_child", "node_children"),
        ("child", "children"),

        ("experience", "experiences"),
        ("day", "days"),

        ("comment", "comments"),
        ("foobar", "foobars"),
        ("newsletter", "newsletters"),

        ("old_news", "old_news"),
        ("news", "news"),

        ("series", "series"),
        ("species", "species"),

        ("quiz", "quizzes"),

        ("perspective", "perspectives"),

        ("ox", "oxen"),
        ("photo", "photos"),
        ("buffalo", "buffaloes"),
        ("tomato", "tomatoes"),
        ("dwarf", "dwarves"),
        ("elf", "elves"),
        ("information", "information"),
        ("equipment", "equipment"),
        ("bus", "buses"),
        ("status", "statuses"),
        ("mouse", "mice"),

        ("louse", "lice"),
        ("house", "houses"),
        ("octopus", "octopi"),
        ("virus", "viri"),
        ("alias", "aliases"),
        ("portfolio", "portfolios"),

        ("vertex", "vertices"),
        ("matrix", "matrices"),

        ("axis", "axes"),
        ("testis", "testes"),
        ("crisis", "crises"),

        ("rice", "rice"),
        ("shoe", "shoes"),

        ("horse", "horses"),
        ("prize", "prizes"),
        ("edge", "edges"),
        ("person", "people"),
        ("student_and_teacher", "student_and_teachers"),
        ("money", "money"),
        ("pretty_fish", "pretty_fish")
    ]
