import unittest
from datetime import date, datetime
from decimal import Decimal

from sweet.sequel.terms.values_list import ValuesList
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestValues(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_values(self):
        d1 = datetime(2024, 12, 13, 11, 9, 28, 547992)
        d2 = datetime(2024, 12, 13, 11, 9, 28, 547992)
        d3 = datetime(2024, 12, 13, 11, 9, 28, 547992)

        vs = ValuesList(1, "lucy", 30, d1)
        self.assertEqual([(1, "lucy", 30, d1)], vs.data)

        vs.append([(2, "lily", 20, d2)])
        self.assertEqual([(1, "lucy", 30, d1), (2, "lily", 20, d2)], vs.data)

        vs.append([(3, "jim", 12, d3), (4, "noname", 9, d1)])
        self.assertEqual([(1, "lucy", 30, d1), (2, "lily", 20, d2), (3, "jim", 12, d3), (4, "noname", 9, d1)], vs.data)

    vs = ValuesList(None, "lucy", True,
                    Decimal(10.000005), 30, 24.7,
                    datetime(2024, 12, 13, 11, 9, 28, 547992),
                    date(2024, 12, 13),
                    b"hello, world", ["list1", None, 1], ("tuple2", None, 2.1)
                    )

    def test_sql__none(self):
        vs = ValuesList(None)
        self.assertEqual("(NULL)", self.mysql.sql(vs))
        self.assertEqual("(NULL)", self.sqlite.sql(vs))
        self.assertEqual("(NULL)", self.pg.sql(vs))

    def test_sql__str_and_bytes(self):
        vs = ValuesList( "lucy", b"hello, world")
        self.assertEqual("('lucy', 'hello, world')", self.mysql.sql(vs))
        self.assertEqual("('lucy', 'hello, world')", self.sqlite.sql(vs))
        self.assertEqual("('lucy', 'hello, world')", self.pg.sql(vs))

    def test_sql__bool_and_number(self):
        vs = ValuesList(True, Decimal(10.000005), 30, 24.7)
        self.assertEqual("(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)", self.mysql.sql(vs))
        self.assertEqual("(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)", self.sqlite.sql(vs))
        self.assertEqual("(1, 10.0000049999999998107114151935093104839324951171875, 30, 24.7)", self.pg.sql(vs))

    def test_sql__date_and_datetime(self):
        vs = ValuesList(datetime(2024, 12, 13, 11, 9, 28, 547992), date(2024, 12, 13))
        self.assertEqual("('2024-12-13 11:09:28', '2024-12-13')", self.mysql.sql(vs))
        self.assertEqual("('2024-12-13 11:09:28', '2024-12-13')", self.sqlite.sql(vs))
        self.assertEqual("('2024-12-13 11:09:28', '2024-12-13')", self.pg.sql(vs))

    def test_sql__tuple_and_list(self):
        vs = ValuesList(["list1", None, 1], ("tuple2", None, 2.1))
        self.assertEqual("(['list1', NULL, 1], ['tuple2', NULL, 2.1])", self.mysql.sql(vs))
        self.assertEqual("(['list1', NULL, 1], ['tuple2', NULL, 2.1])", self.sqlite.sql(vs))
        self.assertEqual("(['list1', NULL, 1], ['tuple2', NULL, 2.1])", self.pg.sql(vs))


if __name__ == '__main__':
    unittest.main()
