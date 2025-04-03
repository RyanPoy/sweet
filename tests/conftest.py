import pytest
import pytest_asyncio

from sweet.database.driver.base_driver import BaseDriver
from sweet.environment import Environment
from sweet import model
from sweet.model import Objects, consts
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor
from tests.helper import settings_mysql, settings_postgresql, settings_sqlite


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


@pytest_asyncio.fixture
async def using():
    async def init(env: Environment, do_connect=True) -> BaseDriver:
        db = env.db_driver(**env.db_settings)
        if do_connect:
            await db.init_pool()
        setattr(Objects, consts.db_adapter, db)
        setattr(Objects, consts.sql_visitor, env.sql_visitor)
        return db

    for env in [Environment(settings_mysql), Environment(settings_sqlite), Environment(settings_postgresql)]:
        driver = None
        try:
            driver = await init(env)
            yield driver
        finally:
            if driver:
                await driver.close_pool()


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
