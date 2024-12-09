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

        sql = manager.to_sql(self.mysql)
        self.assertEqual('INSERT INTO "users" VALUES (*)', sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual('INSERT INTO "users" VALUES (*)', sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual('INSERT INTO "users" VALUES (*)', sql)

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

        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES ('1', 'david'), ('2', 'kir'), ('3', DEFAULT)""", str(sql))


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

        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES (*), (DEFAULT)""", sql)

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
        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("name") VALUES ('david'), ('kir'), (DEFAULT)""", sql)

    def test_inserts_false(self):
        table = Table("users")
        table["bool"] = Column("bool")

        manager = InsertManager()
        manager.insert([[table["bool"], False]])

        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("bool") VALUES (0)""", sql)

    def test_inserts_null(self):
        table = Table("users")
        table["id"] = Column("id")

        manager = InsertManager()
        manager.insert([[table["id"], None]])

        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("id") VALUES (NULL)""", sql)

    def test_inserts_time(self):
        table = Table("users")
        table["created_at"] = Column("created_at")
        manager = InsertManager()

        t = time.time()
        manager.insert([[table["created_at"], t]])

        sql = manager.to_sql(self.mysql)
        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual(f"""INSERT INTO "users" ("created_at") VALUES ({str(t)})""", sql)

    def test_insert_takes_a_list_of_lists(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.into(table)
        manager.insert([[table["id"], 1], [table["name"], "aaron"]])
        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)

    def test_defaults_the_table(self):
        table = Table("users")
        table["id"] = Column("id")
        table["name"] = Column("name")

        manager = InsertManager()
        manager.insert([[table["id"], 1], [table["name"], "aaron"]])

        sql = manager.to_sql(self.mysql)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)

        sql = manager.to_sql(self.sqlite)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)

        sql = manager.to_sql(self.pg)
        self.assertEqual("""INSERT INTO "users" ("id", "name") VALUES (1, 'aaron')""", sql)


    #   it "noop for empty list" do
    #     table = Table("users")
    #     manager = InsertManager()
    #     manager.insert [[table[:id], 1]]
    #     manager.insert []
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" ("id") VALUES (1)
    #     }
    #   end
    #
    #   it "is chainable" do
    #     table = Table("users")
    #     manager = InsertManager()
    #     insert_result = manager.insert [[table[:id], 1]]
    #     assert_equal manager, insert_result
    #   end
    # end
    #
    # describe "into" do
    #   it "takes a Table and chains" do
    #     manager = InsertManager()
    #     _(manager.into(Table.new(:users))).must_equal manager
    #   end
    #
    #   it "converts to sql" do
    #     table   = Table.new :users
    #     manager = InsertManager()
    #     manager.into(table)
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users"
    #     }
    #   end
    # end
    #
    # describe "columns" do
    #   it "converts to sql" do
    #     table   = Table.new :users
    #     manager = InsertManager()
    #     manager.into(table)
    #     manager.columns << table[:id]
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" ("id")
    #     }
    #   end
    # end
    #
    # describe "values" do
    #   it "converts to sql" do
    #     table   = Table.new :users
    #     manager = InsertManager()
    #     manager.into(table)
    #
    #     manager.values = Nodes::ValuesList.new([[1], [2]])
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" VALUES (1), (2)
    #     }
    #   end
    #
    #   it "accepts sql literals" do
    #     table   = Table.new :users
    #     manager = InsertManager()
    #     manager.into(table)
    #
    #     manager.values = sequel.sql("DEFAULT VALUES")
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" DEFAULT VALUES
    #     }
    #   end
    # end
    #
    # describe "combo" do
    #   it "combines columns and values list in order" do
    #     table   = Table.new :users
    #     manager = InsertManager()
    #     manager.into(table)
    #
    #     manager.values = Nodes::ValuesList.new([[1, "aaron"], [2, "david"]])
    #     manager.columns << table[:id]
    #     table["name"] = Column("name"); manager.columns.append(table["name"])
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" ("id", "name") VALUES (1, 'aaron'), (2, 'david')
    #     }
    #   end
    # end
    #
    # describe "select" do
    #   it "accepts a select query in place of a VALUES clause" do
    #     table   = Table.new :users
    #
    #     manager = InsertManager()
    #     manager.into(table)
    #
    #     select = Arel::SelectManager.new
    #     select.project sequel.sql("1")
    #     select.project sequel.sql('"aaron"')
    #
    #     manager.select select
    #     manager.columns << table[:id]
    #     table["name"] = Column("name"); manager.columns.append(table["name"])
    #     _(manager.to_sql).must_be_like %{
    #       INSERT INTO "users" ("id", "name") (SELECT 1, "aaron")
    #     }
    #   end
