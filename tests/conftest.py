from contextlib import asynccontextmanager

import pytest
import pytest_asyncio

from sweet.environment import Environment
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor
from tests.helper import settings_mysql, settings_postgresql, settings_sqlite
from tests.helper.sqls import mysql_sql, postgres_sql, sqlite_sql


class ObjDict(dict):

    def __getattr__(self, key):
        try:
            return self[key]
        except KeyError:
            raise AttributeError(f"'{self.__class__.__name__}' object has no attribute '{key}'")

    def __setattr__(self, key, value):
        self[key] = value


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
            await env.db.execute(sql)

        yield env

        for sql in drop_sqls:
            await env.db.execute(sql)
    finally:
        await env.release_db()


@pytest_asyncio.fixture
async def mysql_env():
    print("*" * 20, "mysql")
    async with _env(settings_mysql, mysql_sql.CREATE_SQLS, mysql_sql.DROP_SQLS) as env:
        yield env


@pytest_asyncio.fixture
async def sqlite_env():
    print("*" * 20, "sqlite")
    async with _env(settings_sqlite, sqlite_sql.CREATE_SQLS, sqlite_sql.DROP_SQLS) as env:
        yield env


@pytest_asyncio.fixture
async def pg_env():
    print("*" * 20, "pg")
    async with _env(settings_postgresql, postgres_sql.CREATE_SQLS, postgres_sql.DROP_SQLS) as env:
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
