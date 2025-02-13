import unittest
from datetime import datetime
from decimal import Decimal

from sweet.sequel.terms.name_fn import Sum, Name
from sweet.sequel.types import Array, Raw
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestRaw(unittest.TestCase):
    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_init_error(self):
        customerObj = object()
        with self.assertRaises(TypeError) as ctx:
            Raw(customerObj)
        self.assertEqual("Raw initialize accept typing.Union[NoneType, bool, str, bytes, int, float, decimal.Decimal, datetime.date, datetime.datetime], "
                         "but got 'object'", str(ctx.exception))

    def test_quote(self):
        self.assertEqual("NULL", Raw(None).quote())
        self.assertEqual("1", Raw(True).quote())
        self.assertEqual("0", Raw(False).quote())
        self.assertEqual("'jim'", Raw("jim").quote())
        self.assertEqual("'abcef'", Raw(b"abcef").quote())
        self.assertEqual("1", Raw(1).quote())
        self.assertEqual("1.5", Raw(1.5).quote())
        self.assertEqual("12.75", Raw(Decimal(12.75)).quote())

        s = '2025-02-13'
        day = datetime.strptime(s, "%Y-%m-%d").date()
        self.assertEqual("'2025-02-13'", Raw(day).quote())

        s = '2025-02-13 10:03:20'
        dt = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
        self.assertEqual("'2025-02-13 10:03:20'", Raw(dt).quote())

    def test_sql(self):
        self.assertEqual("NULL", self.mysql.sql(Raw(None)))
        self.assertEqual("NULL", self.sqlite.sql(Raw(None)))
        self.assertEqual("NULL", self.pg.sql(Raw(None)))

        self.assertEqual("1", self.mysql.sql(Raw(True)))
        self.assertEqual("1", self.sqlite.sql(Raw(True)))
        self.assertEqual("1", self.pg.sql(Raw(True)))

        self.assertEqual("0", self.mysql.sql(Raw(False)))
        self.assertEqual("0", self.sqlite.sql(Raw(False)))
        self.assertEqual("0", self.pg.sql(Raw(False)))

        self.assertEqual("'jim'", self.mysql.sql(Raw("jim")))
        self.assertEqual("'jim'", self.sqlite.sql(Raw("jim")))
        self.assertEqual("'jim'", self.pg.sql(Raw("jim")))

        self.assertEqual("'abcef'", self.mysql.sql(Raw(b"abcef")))
        self.assertEqual("'abcef'", self.sqlite.sql(Raw(b"abcef")))
        self.assertEqual("'abcef'", self.pg.sql(Raw(b"abcef")))

        self.assertEqual("1", self.mysql.sql(Raw(1)))
        self.assertEqual("1", self.sqlite.sql(Raw(1)))
        self.assertEqual("1", self.pg.sql(Raw(1)))

        self.assertEqual("1.5", self.mysql.sql(Raw(1.5)))
        self.assertEqual("1.5", self.sqlite.sql(Raw(1.5)))
        self.assertEqual("1.5", self.pg.sql(Raw(1.5)))

        self.assertEqual("12.75", self.mysql.sql(Raw(Decimal(12.75))))
        self.assertEqual("12.75", self.sqlite.sql(Raw(Decimal(12.75))))
        self.assertEqual("12.75", self.pg.sql(Raw(Decimal(12.75))))

        s = '2025-02-13'
        day = datetime.strptime(s, "%Y-%m-%d").date()
        self.assertEqual("'2025-02-13'", self.mysql.sql(Raw(day)))
        self.assertEqual("'2025-02-13'", self.sqlite.sql(Raw(day)))
        self.assertEqual("'2025-02-13'", self.pg.sql(Raw(day)))

        s = '2025-02-13 10:03:20'
        dt = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
        self.assertEqual("'2025-02-13 10:03:20'", self.mysql.sql(Raw(dt)))
        self.assertEqual("'2025-02-13 10:03:20'", self.sqlite.sql(Raw(dt)))
        self.assertEqual("'2025-02-13 10:03:20'", self.pg.sql(Raw(dt)))


class TestArray(unittest.TestCase):
    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_init(self):
        fn = Sum("age")
        name = Name("username")
        s = '2025-02-13 10:03:20'
        now = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
        array = Array([1, "1", now, fn, name, [1, "1", now, fn, name, [1, "1", now, fn, name]]])
        self.assertEqual(array.data, [Raw(1), Raw("1"), Raw(now), fn, name, [Raw(1), Raw("1"), Raw(now), fn, name, [Raw(1), Raw("1"), Raw(now), fn, name]]])

    def test_sql(self):
        fn = Sum("age")
        name = Name("username")
        s = '2025-02-13 10:03:20'
        now = datetime.strptime(s, "%Y-%m-%d %H:%M:%S")
        array = Array([1, "1", now, fn, name, [1, "1", now, fn, name, [1, "1", now, fn, name]]])
        self.assertEqual("(1, '1', '2025-02-13 10:03:20', SUM(`age`), `username`, (1, '1', '2025-02-13 10:03:20', SUM(`age`), `username`, (1, '1', "
                         "'2025-02-13 10:03:20', SUM(`age`), `username`)))", self.mysql.sql(array))
        self.assertEqual("(1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', "
                         "'2025-02-13 10:03:20', SUM(\"age\"), \"username\")))", self.sqlite.sql(array))
        self.assertEqual("(1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', '2025-02-13 10:03:20', SUM(\"age\"), \"username\", (1, '1', "
                         "'2025-02-13 10:03:20', SUM(\"age\"), \"username\")))", self.pg.sql(array))


if __name__ == '__main__':
    unittest.main()

