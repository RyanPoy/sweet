import time
import unittest
import sweet.sequel as sequel
from sweet.sequel import MySQL, PostgreSQL, SQLite
from sweet.sequel.managers.insert_manager import InsertManager
from sweet.sequel.nodes.values_list import ValuesList
from sweet.sequel.schema.column import Column
from sweet.sequel.schema.table import Table


class TestInsertManager(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQL()
        self.sqlite = SQLite()
        self.pg = PostgreSQL()

    def test_create_a_ValuesList_node(self):
        manager = InsertManager()
        values = manager.create_values_list([["a", "b"], ["c", "d"]])

        self.assertTrue(isinstance(values, ValuesList))
        self.assertEqual([["a", "b"], ["c", "d"]], values.rows)

    def test_allows_sql_literals(self):
        manager = InsertManager()
        tabel = Table("users")
        manager.into(tabel)
        manager.values = manager.create_values([sequel.sql("*")])

        self.assertEqual('INSERT INTO "users" VALUES (*)', manager.to_sql(self.mysql))

        self.assertEqual('INSERT INTO "users" VALUES (*)', manager.to_sql(self.sqlite))

        self.assertEqual('INSERT INTO "users" VALUES (*)', manager.to_sql(self.pg))

    def test_works_with_multiple_values(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)

        manager.columns.append(table["id"])
        manager.columns.append(table["name"])

        manager.values = manager.create_values_list([
            ["1", "david"],
            ["2", "kir"],
            ["3", sequel.sql("DEFAULT")],
        ])
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", manager.to_sql(self.pg))

    def test_literals_in_multiple_values_are_not_escaped(self):
        table = Table("users")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)
        manager.columns.append(table["name"])

        manager.values = manager.create_values_list([
            [sequel.sql("*")],
            [sequel.sql("DEFAULT")],
        ])

        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", manager.to_sql(self.pg))

    def test_works_with_multiple_single_values(self):
        table = Table("users")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)

        manager.columns.append(table["name"])

        manager.values = manager.create_values_list([
            ["david"],
            ["kir"],
            [sequel.sql("DEFAULT")],
        ])
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", manager.to_sql(self.pg))

    def test_inserts_false(self):
        table = Table("users")
        table["bool"] = Column("bool")

        manager = InsertManager()
        manager.insert([[table["bool"], False]])

        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", manager.to_sql(self.pg))

    def test_inserts_null(self):
        table = Table("users")
        table["id"] = Column("id")

        manager = InsertManager()
        manager.insert([[table["id"], None]])

        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", manager.to_sql(self.pg))

    def test_inserts_time(self):
        table = Table("users")
        table["created_at"] = Column("created_at")
        manager = InsertManager()

        t = time.time()
        manager.insert([[table["created_at"], t]])

        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", manager.to_sql(self.mysql))
        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", manager.to_sql(self.sqlite))
        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", manager.to_sql(self.pg))

    def test_insert_takes_a_list_of_lists(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)
        manager.insert([[table["id"], 1], [table["name"], "aaron"]])
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.pg))

    def test_defaults_the_table(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.insert([[table["id"], 1], [table["name"], "aaron"]])

        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", manager.to_sql(self.pg))

    def test_noop_for_empty_list(self):
        table = Table("users")
        table["id"] = Column("id")

        manager = InsertManager()
        manager.insert([[table["id"], 1]])
        manager.insert([])
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (1)""", manager.to_sql(self.mysql))
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (1)""", manager.to_sql(self.sqlite))
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (1)""", manager.to_sql(self.pg))

    def test_is_chainable(self):
        table = Table("users")
        table["id"] = Column("id")

        manager = InsertManager()
        insert_result = manager.insert([[table["id"], 1]])
        self.assertIs(manager, insert_result)

    def test_takes_a_table_and_chains_after_call_into(self):
        manager = InsertManager()
        self.assertTrue(isinstance(manager.into(Table("users")), InsertManager))

    def test_converts_to_sql_after_call_into(self):
        table = Table("users")
        manager = InsertManager()
        manager.into(table)
        self.assertEqual('''INSERT INTO "users"''', manager.to_sql(self.mysql))
        self.assertEqual('''INSERT INTO "users"''', manager.to_sql(self.sqlite))
        self.assertEqual('''INSERT INTO "users"''', manager.to_sql(self.pg))

    def test_convers_to_sql_after_call_columns(self):
        table = Table("users")
        table["id"] = Column("id")

        manager = InsertManager()
        manager.into(table)
        manager.columns.append(table["id"])
        self.assertEqual('INSERT INTO "users" ("id")', manager.to_sql(self.mysql))
        self.assertEqual('INSERT INTO "users" ("id")', manager.to_sql(self.sqlite))
        self.assertEqual('INSERT INTO "users" ("id")', manager.to_sql(self.pg))

    def test_converts_to_sql_after_set_values(self):
        table = Table("users")

        manager = InsertManager()
        manager.into(table)
        manager.values = ValuesList([[1], [2]])

        self.assertEqual('''INSERT INTO "users" VALUES (1), (2)''', manager.to_sql(self.mysql))
        self.assertEqual('''INSERT INTO "users" VALUES (1), (2)''', manager.to_sql(self.sqlite))
        self.assertEqual('''INSERT INTO "users" VALUES (1), (2)''', manager.to_sql(self.pg))

    def test_accepts_set_sql_literals(self):
        table = Table("users")
        manager = InsertManager()
        manager.into(table)

        manager.values = sequel.sql("DEFAULT VALUES")
        self.assertEqual('''INSERT INTO "users" DEFAULT VALUES''', manager.to_sql(self.mysql))
        self.assertEqual('''INSERT INTO "users" DEFAULT VALUES''', manager.to_sql(self.sqlite))
        self.assertEqual('''INSERT INTO "users" DEFAULT VALUES''', manager.to_sql(self.pg))

    def test_combines_columns_and_values_list_in_order(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)

        manager.values = ValuesList([[1, "aaron"], [2, "david"]])
        manager.columns.append(table["id"])
        manager.columns.append(table["name"])

        self.assertEqual('''INSERT INTO "users" ("id", "name") VALUES (1, 'aaron'), (2, 'david')''', manager.to_sql(self.mysql))
        self.assertEqual('''INSERT INTO "users" ("id", "name") VALUES (1, 'aaron'), (2, 'david')''', manager.to_sql(self.sqlite))
        self.assertEqual('''INSERT INTO "users" ("id", "name") VALUES (1, 'aaron'), (2, 'david')''', manager.to_sql(self.pg))

    # def test_accepts_a_select_query_in_place_of_a_VALUES_clause(self):
    #     # Todo: pass this test
    #     table = Table("users")
    #     table["id"] = Column("id")
    #     table["name"] = Column("name")
    #
    #     manager = InsertManager()
    #     manager.into(table)
    #
    #     select = SelectManager()
    #     select.project(sequel.sql("1"))
    #     select.project(sequel.sql('"aaron"'))
    #
    #     manager.select = select
    #
    #     manager.columns.append(table["id"])
    #     manager.columns.append(table["name"])
    #
    #     self.assertEqual('''INSERT INTO "users" ("id", "name") (SELECT 1, "aaron")''', manager.to_sql(self.sqlite))
    #     self.assertEqual('''INSERT INTO "users" ("id", "name") (SELECT 1, "aaron")''', manager.to_sql(self.mysql))
    #     self.assertEqual('''INSERT INTO "users" ("id", "name") (SELECT 1, "aaron")''', manager.to_sql(self.pg))

#   def test_insert_simple(self):
#         sql, params = self.users_mysql.insert(username='charlie', superuser=0, admin=1).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s)', sql)
#         self.assertEqual(['charlie', 0, 1], params)
#
#         sql, params = self.users_sqlite.insert(username='charlie', superuser=0, admin=1).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?)', sql)
#         self.assertEqual(['charlie', 0, 1], params)
#
#         sql, params = self.users_pg.insert(username='charlie', superuser=0, admin=1).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3)', sql)
#         self.assertEqual(['charlie', 0, 1], params)
#
#     def test_insert_list(self):
#         data = [
#             {'username': 'charlie', 'superuser': 0, 'admin': 1},
#             {'username': 'lucy', 'superuser': 0, 'admin': 0},
#             {'username': 'jim', 'superuser': 1, 'admin': 0},
#             {'username': 'mary', 'superuser': 1, 'admin': 1},
#         ]
#         sql, params = self.users_mysql.insert(data).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s), (%s, %s, %s), (%s, %s, %s), (%s, %s, %s)', sql)
#         self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)
#
#         sql, params = self.users_sqlite.insert(data).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?), (?, ?, ?), (?, ?, ?), (?, ?, ?)', sql)
#         self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)
#
#         sql, params = self.users_pg.insert(data).sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3), ($4, $5, $6), ($7, $8, $9), ($10, $11, $12)', sql)
#         self.assertEqual(['charlie', 0, 1, 'lucy', 0, 0, 'jim', 1, 0, 'mary', 1, 1], params)
#
#     def test_insert_returning(self):
#         data = [
#             {'username': 'charlie', 'superuser': 0, 'admin': 1},
#             {'username': 'mary', 'superuser': 1, 'admin': 1},
#         ]
#         sql, params = self.users_mysql.insert(data).returning('username', 'admin').sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (%s, %s, %s), (%s, %s, %s) RETURNING ("username", "admin")', sql)
#         self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)
#
#         sql, params = self.users_sqlite.insert(data).returning('username', 'admin').sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES (?, ?, ?), (?, ?, ?) RETURNING ("username", "admin")', sql)
#         self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)
#
#         sql, params = self.users_pg.insert(data).returning('username', 'admin').sql()
#         self.assertEqual('INSERT INTO "users" ("username", "superuser", "admin") VALUES ($1, $2, $3), ($4, $5, $6) RETURNING ("username", "admin")', sql)
#         self.assertEqual(['charlie', 0, 1, 'mary', 1, 1], params)

    # @requires_sqlite
    # def test_replace_sqlite(self):
    #     query = User.replace({
    #         User.c.username: 'charlie',
    #         User.c.superuser: False})
    #     self.assertSQL(query, (
    #         'INSERT OR REPLACE INTO "users" ("superuser", "username") '
    #         'VALUES (?, ?)'), [False, 'charlie'])
    #
    # @requires_mysql
    # def test_replace_mysql(self):
    #     query = User.replace({
    #         User.c.username: 'charlie',
    #         User.c.superuser: False})
    #     self.assertSQL(query, (
    #         'REPLACE INTO "users" ("superuser", "username") '
    #         'VALUES (?, ?)'), [False, 'charlie'])
