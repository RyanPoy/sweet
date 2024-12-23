import unittest

from sweet.sequel.terms.q import Q
from sweet.sequel.visitors.mysql_visitor import MySQLVisitor
from sweet.sequel.visitors.postgresql_visitor import PostgreSQLVisitor
from sweet.sequel.visitors.sqlite_visitor import SQLiteVisitor


class TestQ(unittest.TestCase):

    def setUp(self):
        self.mysql = MySQLVisitor()
        self.sqlite = SQLiteVisitor()
        self.pg = PostgreSQLVisitor()

    def test_combine_empty_copy(self):
        q = Q(x=1)
        self.assertEqual(q, q | Q())
        self.assertEqual(q, Q() | q)
        self.assertEqual(q, q & Q())
        self.assertEqual(q, Q() & q)

    def test_combine_and_both_empty(self):
        self.assertEqual(Q() & Q(), Q())

    def test_combine_or_both_empty(self):
        self.assertEqual(Q() | Q(), Q())

    def test_combine_not_q_object(self):
        with self.assertRaises(TypeError):
            Q(x=1) | object()
        with self.assertRaises(TypeError) as ctx:
            Q(x=1) & object()
        self.assertEqual("Logical operators can only be applied between two Q objects.", str(ctx.exception))

    def test_or(self):
        q = Q(price__gt="discounted_price") | Q(price="discounted_price")
        self.assertEqual("""("price" > 'discounted_price' OR "price" = 'discounted_price')""", q.sql(self.mysql))
        self.assertEqual("""("price" > 'discounted_price' OR "price" = 'discounted_price')""", q.sql(self.sqlite))
        self.assertEqual("""("price" > 'discounted_price' OR "price" = 'discounted_price')""", q.sql(self.pg))

    def test_and(self):
        q = Q(price__gt="discounted_price") & Q(price="discounted_price")
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.mysql))
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.sqlite))
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.pg))

    def test_deconstruct_multiple_kwargs(self):
        q = Q(price__gt="discounted_price", price="discounted_price")
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.mysql))
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.sqlite))
        self.assertEqual("""("price" > 'discounted_price' AND "price" = 'discounted_price')""", q.sql(self.pg))

    def test_complex_logic(self):
        q = ( Q(price__gte="discounted_price") | Q(price="discounted_price") ) \
             & ( Q(age__lte=20) | Q(name="abc") )
        self.assertEqual("""(("price" >= 'discounted_price' OR "price" = 'discounted_price') AND ("age" <= 20 OR "name" = 'abc'))""", q.sql(self.mysql))
        self.assertEqual("""(("price" >= 'discounted_price' OR "price" = 'discounted_price') AND ("age" <= 20 OR "name" = 'abc'))""", q.sql(self.sqlite))
        self.assertEqual("""(("price" >= 'discounted_price' OR "price" = 'discounted_price') AND ("age" <= 20 OR "name" = 'abc'))""", q.sql(self.pg))

    def test_equal(self):
        self.assertEqual(Q(), Q())
        self.assertEqual( Q(pk=(1, 2)), Q(pk=(1, 2)) )
        self.assertEqual( Q(pk=(1, 2)), Q(pk=[1, 2]) )

    def test_hash(self):
        self.assertEqual(hash(Q()), hash(Q()))
        self.assertEqual( hash(Q(pk=(1, 2))), hash(Q(pk=(1, 2))) )
        self.assertEqual( hash(Q(pk=(1, 2))), hash(Q(pk=[1, 2])) )


if __name__ == '__main__':
    unittest.main()
