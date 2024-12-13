import unittest

from sweet.arel import SQLite, PostgreSQL, MySQL
from sweet.arel.managers.delete_manager import DeleteManager
from sweet.arel.schema.column import Column
from sweet.arel.schema.table import Table


class DeleteManagerTest(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQL()
        self.sqlite = SQLite()
        self.pg = PostgreSQL()

    def test_handles_limit_properly(self):
        table = Table("users")
        table["id"] = Column("id")

        dm = DeleteManager()
        dm.take(10)
        dm.from_(table)
        dm.key = table["id"]

        self.assertEqual('DELETE FROM "users" WHERE 1=0', dm.to_sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE 1=0', dm.to_sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE 1=0', dm.to_sql(self.pg))

    def test_uses_from(self):
        table = Table("users")
        dm = DeleteManager()
        dm.from_(table)

        self.assertEqual('DELETE FROM "users"', dm.to_sql(self.mysql))
        self.assertEqual('DELETE FROM "users"', dm.to_sql(self.sqlite))
        self.assertEqual('DELETE FROM "users"', dm.to_sql(self.pg))

    def test_chains(self):
        table = Table("users")
        dm = DeleteManager()
        self.assertTrue(dm.from_(table) == dm)

    def test_uses_where_values(self):
        table = Table("users")
        table["id"] = Column("id")
        dm = DeleteManager()
        dm.from_(table)
        dm.where(table["id"].eq(10))
        self.assertEqual('DELETE FROM "users" WHERE "users"."id" = 10', dm.to_sql(self.mysql))
        self.assertEqual('DELETE FROM "users" WHERE "users"."id" = 10', dm.to_sql(self.sqlite))
        self.assertEqual('DELETE FROM "users" WHERE "users"."id" = 10', dm.to_sql(self.pg))

#       it "chains" do
#         table = Table("users")
#         dm = DeleteManager()
#         _(dm.where(table["id"].eq(10))).must_equal dm
#       end
#     end
#   end
