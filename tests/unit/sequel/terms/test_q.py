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

    def test_deconstruct_or(self):
        q1 = Q(price__gt="discounted_price")
        q2 = Q(price="discounted_price")
        q = q1 | q2
        from pprint import pprint as pp
        print("*"*10)
        print(q)
        print("*"*10)

#     def test_deconstruct_xor(self):
#         q1 = Q(price__gt=F("discounted_price"))
#         q2 = Q(price=F("discounted_price"))
#         q = q1 ^ q2
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(
#             args,
#             (
#                 ("price__gt", F("discounted_price")),
#                 ("price", F("discounted_price")),
#             ),
#         )
#         self.assertEqual(kwargs, {"_connector": Q.XOR})
#
#     def test_deconstruct_and(self):
#         q1 = Q(price__gt=F("discounted_price"))
#         q2 = Q(price=F("discounted_price"))
#         q = q1 & q2
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(
#             args,
#             (
#                 ("price__gt", F("discounted_price")),
#                 ("price", F("discounted_price")),
#             ),
#         )
#         self.assertEqual(kwargs, {})
#
#     def test_deconstruct_multiple_kwargs(self):
#         q = Q(price__gt=F("discounted_price"), price=F("discounted_price"))
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(
#             args,
#             (
#                 ("price", F("discounted_price")),
#                 ("price__gt", F("discounted_price")),
#             ),
#         )
#         self.assertEqual(kwargs, {})
#
#     def test_deconstruct_nested(self):
#         q = Q(Q(price__gt=F("discounted_price")))
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(args, (Q(price__gt=F("discounted_price")),))
#         self.assertEqual(kwargs, {})
#
#     def test_deconstruct_boolean_expression(self):
#         expr = RawSQL("1 = 1", BooleanField())
#         q = Q(expr)
#         _, args, kwargs = q.deconstruct()
#         self.assertEqual(args, (expr,))
#         self.assertEqual(kwargs, {})
#
#     def test_reconstruct(self):
#         q = Q(price__gt=F("discounted_price"))
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(Q(*args, **kwargs), q)
#
#     def test_reconstruct_negated(self):
#         q = ~Q(price__gt=F("discounted_price"))
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(Q(*args, **kwargs), q)
#
#     def test_reconstruct_or(self):
#         q1 = Q(price__gt=F("discounted_price"))
#         q2 = Q(price=F("discounted_price"))
#         q = q1 | q2
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(Q(*args, **kwargs), q)
#
#     def test_reconstruct_xor(self):
#         q1 = Q(price__gt=F("discounted_price"))
#         q2 = Q(price=F("discounted_price"))
#         q = q1 ^ q2
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(Q(*args, **kwargs), q)
#
#     def test_reconstruct_and(self):
#         q1 = Q(price__gt=F("discounted_price"))
#         q2 = Q(price=F("discounted_price"))
#         q = q1 & q2
#         path, args, kwargs = q.deconstruct()
#         self.assertEqual(Q(*args, **kwargs), q)
#
#     def test_equal(self):
#         self.assertEqual(Q(), Q())
#         self.assertEqual(
#             Q(("pk__in", (1, 2))),
#             Q(("pk__in", [1, 2])),
#         )
#         self.assertEqual(
#             Q(("pk__in", (1, 2))),
#             Q(pk__in=[1, 2]),
#         )
#         self.assertEqual(
#             Q(("pk__in", (1, 2))),
#             Q(("pk__in", {1: "first", 2: "second"}.keys())),
#         )
#         self.assertNotEqual(
#             Q(name__iexact=F("other_name")),
#             Q(name=Lower(F("other_name"))),
#         )
#
#     def test_hash(self):
#         self.assertEqual(hash(Q()), hash(Q()))
#         self.assertEqual(
#             hash(Q(("pk__in", (1, 2)))),
#             hash(Q(("pk__in", [1, 2]))),
#         )
#         self.assertEqual(
#             hash(Q(("pk__in", (1, 2)))),
#             hash(Q(pk__in=[1, 2])),
#         )
#         self.assertEqual(
#             hash(Q(("pk__in", (1, 2)))),
#             hash(Q(("pk__in", {1: "first", 2: "second"}.keys()))),
#         )
#         self.assertNotEqual(
#             hash(Q(name__iexact=F("other_name"))),
#             hash(Q(name=Lower(F("other_name")))),
#         )
#
#     def test_flatten(self):
#         q = Q()
#         self.assertEqual(list(q.flatten()), [q])
#         q = Q(NothingNode())
#         self.assertEqual(list(q.flatten()), [q, q.children[0]])
#         q = Q(
#             ExpressionWrapper(
#                 Q(RawSQL("id = 0", params=(), output_field=BooleanField()))
#                 | Q(price=Value("4.55"))
#                 | Q(name=Lower("category")),
#                 output_field=BooleanField(),
#             )
#         )
#         flatten = list(q.flatten())
#         self.assertEqual(len(flatten), 7)
#
#     def test_create_helper(self):
#         items = [("a", 1), ("b", 2), ("c", 3)]
#         for connector in [Q.AND, Q.OR, Q.XOR]:
#             with self.subTest(connector=connector):
#                 self.assertEqual(
#                     Q.create(items, connector=connector),
#                     Q(*items, _connector=connector),
#                 )
#
#     def test_referenced_base_fields(self):
#         # Make sure Q.referenced_base_fields retrieves all base fields from
#         # both filters and F expressions.
#         tests = [
#             (Q(field_1=1) & Q(field_2=1), {"field_1", "field_2"}),
#             (
#                 Q(Exact(F("field_3"), IsNull(F("field_4"), True))),
#                 {"field_3", "field_4"},
#             ),
#             (Q(Exact(Q(field_5=F("field_6")), True)), {"field_5", "field_6"}),
#             (Q(field_2=1), {"field_2"}),
#             (Q(field_7__lookup=True), {"field_7"}),
#             (Q(field_7__joined_field__lookup=True), {"field_7"}),
#         ]
#         combined_q = Q(1)
#         combined_q_base_fields = set()
#         for q, expected_base_fields in tests:
#             combined_q &= q
#             combined_q_base_fields |= expected_base_fields
#         tests.append((combined_q, combined_q_base_fields))
#         for q, expected_base_fields in tests:
#             with self.subTest(q=q):
#                 self.assertEqual(
#                     q.referenced_base_fields,
#                     expected_base_fields,
#                 )
#
#
# class QCheckTests(TestCase):
#     def test_basic(self):
#         q = Q(price__gt=20)
#         self.assertIs(q.check({"price": 30}), True)
#         self.assertIs(q.check({"price": 10}), False)
#
#     def test_expression(self):
#         q = Q(name="test")
#         self.assertIs(q.check({"name": Lower(Value("TeSt"))}), True)
#         self.assertIs(q.check({"name": Value("other")}), False)
#
#     def test_missing_field(self):
#         q = Q(description__startswith="prefix")
#         msg = "Cannot resolve keyword 'description' into field."
#         with self.assertRaisesMessage(FieldError, msg):
#             q.check({"name": "test"})
#
#     def test_boolean_expression(self):
#         q = Q(ExpressionWrapper(Q(price__gt=20), output_field=BooleanField()))
#         self.assertIs(q.check({"price": 25}), True)
#         self.assertIs(q.check({"price": Value(10)}), False)
#
#     def test_rawsql(self):
#         """
#         RawSQL expressions cause a database error because "price" cannot be
#         replaced by its value. In this case, Q.check() logs a warning and
#         return True.
#         """
#         q = Q(RawSQL("price > %s", params=(20,), output_field=BooleanField()))
#         with self.assertLogs("django.db.models", "WARNING") as cm:
#             self.assertIs(q.check({"price": 10}), True)
#         self.assertIn(
#             f"Got a database error calling check() on {q!r}: ",
#             cm.records[0].getMessage(),
#         )
#
#         # We must leave the connection in a usable state (#35712).
#         self.assertTrue(connection.is_usable())

if __name__ == '__main__':
    unittest.main()
