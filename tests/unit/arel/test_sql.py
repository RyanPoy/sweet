# import datetime
# import re
# 
# from peewee import *
# from peewee import Expression
# from peewee import Function
# from peewee import query_to_string
# 
# from .base import BaseTestCase
# from .base import TestModel
# from .base import db
# from .base import requires_mysql
# from .base import requires_sqlite
# from .base import __sql__
# 
# 
# User = Table('users')
# Tweet = Table('tweets')
# Person = Table('person', ['id', 'name', 'dob'], primary_key='id')
# Note = Table('note', ['id', 'person_id', 'content'])
# 


# Register = Table('register', ('id', 'value', 'category'))
# 
# 
# class TestWindowFunctions(BaseTestCase):
#     def test_partition_unordered(self):
#         partition = [Register.category]
#         query = (Register
#                  .select(
#                      Register.category,
#                      Register.value,
#                      fn.AVG(Register.value).over(partition_by=partition))
#                  .order_by(Register.id))
#         self.assertSQL(query, (
#             'SELECT "t1"."category", "t1"."value", AVG("t1"."value") '
#             'OVER (PARTITION BY "t1"."category") '
#             'FROM "register" AS "t1" ORDER BY "t1"."id"'), [])
# 
#     def test_ordered_unpartitioned(self):
#         query = (Register
#                  .select(
#                      Register.value,
#                      fn.RANK().over(order_by=[Register.value])))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", RANK() OVER (ORDER BY "t1"."value") '
#             'FROM "register" AS "t1"'), [])
# 
#     def test_ordered_partitioned(self):
#         query = Register.select(
#             Register.value,
#             fn.SUM(Register.value).over(
#                 order_by=Register.id,
#                 partition_by=Register.category).alias('rsum'))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", SUM("t1"."value") '
#             'OVER (PARTITION BY "t1"."category" ORDER BY "t1"."id") AS "rsum" '
#             'FROM "register" AS "t1"'), [])
# 
#     def test_empty_over(self):
#         query = (Register
#                  .select(Register.value, fn.LAG(Register.value, 1).over())
#                  .order_by(Register.value))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", LAG("t1"."value", ?) OVER () '
#             'FROM "register" AS "t1" '
#             'ORDER BY "t1"."value"'), [1])
# 
#     def test_frame(self):
#         query = (Register
#                  .select(
#                      Register.value,
#                      fn.AVG(Register.value).over(
#                          partition_by=[Register.category],
#                          start=Window.preceding(),
#                          end=Window.following(2))))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", AVG("t1"."value") '
#             'OVER (PARTITION BY "t1"."category" '
#             'ROWS BETWEEN UNBOUNDED PRECEDING AND 2 FOLLOWING) '
#             'FROM "register" AS "t1"'), [])
# 
#         query = (Register
#                  .select(Register.value, fn.AVG(Register.value).over(
#                      partition_by=[Register.category],
#                      order_by=[Register.value],
#                      start=Window.CURRENT_ROW,
#                      end=Window.following())))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", AVG("t1"."value") '
#             'OVER (PARTITION BY "t1"."category" '
#             'ORDER BY "t1"."value" '
#             'ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) '
#             'FROM "register" AS "t1"'), [])
# 
#     def test_frame_types(self):
#         def assertFrame(over_kwargs, expected):
#             query = Register.select(
#                 Register.value,
#                 fn.SUM(Register.value).over(**over_kwargs))
#             sql, params = __sql__(query)
#             match_obj = re.search(r'OVER \((.*?)\) FROM', sql)
#             self.assertTrue(match_obj is not None)
#             self.assertEqual(match_obj.groups()[0], expected)
#             self.assertEqual(params, [])
# 
#         # No parameters -- empty OVER().
#         assertFrame({}, (''))
#         # Explicitly specify RANGE / ROWS frame-types.
#         assertFrame({'frame_type': Window.RANGE}, 'RANGE UNBOUNDED PRECEDING')
#         assertFrame({'frame_type': Window.ROWS}, 'ROWS UNBOUNDED PRECEDING')
# 
#         # Start and end boundaries.
#         assertFrame({'start': Window.preceding(), 'end': Window.following()},
#                     'ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING')
#         assertFrame({
#             'start': Window.preceding(),
#             'end': Window.following(),
#             'frame_type': Window.RANGE,
#         }, 'RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING')
#         assertFrame({
#             'start': Window.preceding(),
#             'end': Window.following(),
#             'frame_type': Window.ROWS,
#         }, 'ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING')
# 
#         # Start boundary.
#         assertFrame({'start': Window.preceding()}, 'ROWS UNBOUNDED PRECEDING')
#         assertFrame({'start': Window.preceding(), 'frame_type': Window.RANGE},
#                     'RANGE UNBOUNDED PRECEDING')
#         assertFrame({'start': Window.preceding(), 'frame_type': Window.ROWS},
#                     'ROWS UNBOUNDED PRECEDING')
# 
#         # Ordered or partitioned.
#         assertFrame({'order_by': Register.value}, 'ORDER BY "t1"."value"')
#         assertFrame({'frame_type': Window.RANGE, 'order_by': Register.value},
#                     'ORDER BY "t1"."value" RANGE UNBOUNDED PRECEDING')
#         assertFrame({'frame_type': Window.ROWS, 'order_by': Register.value},
#                     'ORDER BY "t1"."value" ROWS UNBOUNDED PRECEDING')
#         assertFrame({'partition_by': Register.category},
#                     'PARTITION BY "t1"."category"')
#         assertFrame({
#             'frame_type': Window.RANGE,
#             'partition_by': Register.category,
#         }, 'PARTITION BY "t1"."category" RANGE UNBOUNDED PRECEDING')
#         assertFrame({
#             'frame_type': Window.ROWS,
#             'partition_by': Register.category,
#         }, 'PARTITION BY "t1"."category" ROWS UNBOUNDED PRECEDING')
# 
#         # Ordering and boundaries.
#         assertFrame({'order_by': Register.value, 'start': Window.CURRENT_ROW,
#                      'end': Window.following()},
#                     ('ORDER BY "t1"."value" '
#                      'ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING'))
#         assertFrame({'order_by': Register.value, 'start': Window.CURRENT_ROW,
#                      'end': Window.following(), 'frame_type': Window.RANGE},
#                     ('ORDER BY "t1"."value" '
#                      'RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING'))
#         assertFrame({'order_by': Register.value, 'start': Window.CURRENT_ROW,
#                      'end': Window.following(), 'frame_type': Window.ROWS},
#                     ('ORDER BY "t1"."value" '
#                      'ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING'))
# 
#     def test_running_total(self):
#         EventLog = Table('evtlog', ('id', 'timestamp', 'data'))
# 
#         w = fn.SUM(EventLog.timestamp).over(order_by=[EventLog.timestamp])
#         query = (EventLog
#                  .select(EventLog.timestamp, EventLog.data, w.alias('elapsed'))
#                  .order_by(EventLog.timestamp))
#         self.assertSQL(query, (
#             'SELECT "t1"."timestamp", "t1"."data", '
#             'SUM("t1"."timestamp") OVER (ORDER BY "t1"."timestamp") '
#             'AS "elapsed" '
#             'FROM "evtlog" AS "t1" ORDER BY "t1"."timestamp"'), [])
# 
#         w = fn.SUM(EventLog.timestamp).over(
#             order_by=[EventLog.timestamp],
#             partition_by=[EventLog.data])
#         query = (EventLog
#                  .select(EventLog.timestamp, EventLog.data, w.alias('elapsed'))
#                  .order_by(EventLog.timestamp))
#         self.assertSQL(query, (
#             'SELECT "t1"."timestamp", "t1"."data", '
#             'SUM("t1"."timestamp") OVER '
#             '(PARTITION BY "t1"."data" ORDER BY "t1"."timestamp") AS "elapsed"'
#             ' FROM "evtlog" AS "t1" ORDER BY "t1"."timestamp"'), [])
# 
#     def test_named_window(self):
#         window = Window(partition_by=[Register.category])
#         query = (Register
#                  .select(
#                      Register.category,
#                      Register.value,
#                      fn.AVG(Register.value).over(window))
#                  .window(window))
# 
#         self.assertSQL(query, (
#             'SELECT "t1"."category", "t1"."value", AVG("t1"."value") '
#             'OVER w '
#             'FROM "register" AS "t1" '
#             'WINDOW w AS (PARTITION BY "t1"."category")'), [])
# 
#         window = Window(
#             partition_by=[Register.category],
#             order_by=[Register.value.desc()])
#         query = (Register
#                  .select(
#                      Register.value,
#                      fn.RANK().over(window))
#                  .window(window))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", RANK() OVER w '
#             'FROM "register" AS "t1" '
#             'WINDOW w AS ('
#             'PARTITION BY "t1"."category" '
#             'ORDER BY "t1"."value" DESC)'), [])
# 
#     def test_multiple_windows(self):
#         w1 = Window(partition_by=[Register.category]).alias('w1')
#         w2 = Window(order_by=[Register.value]).alias('w2')
#         query = (Register
#                  .select(
#                      Register.value,
#                      fn.AVG(Register.value).over(w1),
#                      fn.RANK().over(w2))
#                  .window(w1, w2))
#         self.assertSQL(query, (
#             'SELECT "t1"."value", AVG("t1"."value") OVER w1, RANK() OVER w2 '
#             'FROM "register" AS "t1" '
#             'WINDOW w1 AS (PARTITION BY "t1"."category"), '
#             'w2 AS (ORDER BY "t1"."value")'), [])
# 
#     def test_alias_window(self):
#         w = Window(order_by=Register.value).alias('wx')
#         query = Register.select(Register.value, fn.RANK().over(w)).window(w)
# 
#         # We can re-alias the window and it's updated alias is reflected
#         # correctly in the final query.
#         w.alias('wz')
#         self.assertSQL(query, (
#             'SELECT "t1"."value", RANK() OVER wz '
#             'FROM "register" AS "t1" '
#             'WINDOW wz AS (ORDER BY "t1"."value")'), [])
# 
#     def test_reuse_window(self):
#         EventLog = Table('evt', ('id', 'timestamp', 'key'))
#         window = Window(partition_by=[EventLog.key],
#                         order_by=[EventLog.timestamp])
#         query = (EventLog
#                  .select(EventLog.timestamp, EventLog.key,
#                          fn.NTILE(4).over(window).alias('quartile'),
#                          fn.NTILE(5).over(window).alias('quintile'),
#                          fn.NTILE(100).over(window).alias('percentile'))
#                  .order_by(EventLog.timestamp)
#                  .window(window))
#         self.assertSQL(query, (
#             'SELECT "t1"."timestamp", "t1"."key", '
#             'NTILE(?) OVER w AS "quartile", '
#             'NTILE(?) OVER w AS "quintile", '
#             'NTILE(?) OVER w AS "percentile" '
#             'FROM "evt" AS "t1" '
#             'WINDOW w AS ('
#             'PARTITION BY "t1"."key" ORDER BY "t1"."timestamp") '
#             'ORDER BY "t1"."timestamp"'), [4, 5, 100])
# 
#     def test_filter_clause(self):
#         condsum = fn.SUM(Register.value).filter(Register.value > 1).over(
#             order_by=[Register.id], partition_by=[Register.category],
#             start=Window.preceding(1))
#         query = (Register
#                  .select(Register.category, Register.value, condsum)
#                  .order_by(Register.category))
#         self.assertSQL(query, (
#             'SELECT "t1"."category", "t1"."value", SUM("t1"."value") FILTER ('
#             'WHERE ("t1"."value" > ?)) OVER (PARTITION BY "t1"."category" '
#             'ORDER BY "t1"."id" ROWS 1 PRECEDING) '
#             'FROM "register" AS "t1" '
#             'ORDER BY "t1"."category"'), [1])
# 
#     def test_window_in_orderby(self):
#         Register = Table('register', ['id', 'value'])
#         w = Window(partition_by=[Register.value], order_by=[Register.id])
#         query = (Register
#                  .select()
#                  .window(w)
#                  .order_by(fn.FIRST_VALUE(Register.id).over(w)))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."value" FROM "register" AS "t1" '
#             'WINDOW w AS (PARTITION BY "t1"."value" ORDER BY "t1"."id") '
#             'ORDER BY FIRST_VALUE("t1"."id") OVER w'), [])
# 
#         fv = fn.FIRST_VALUE(Register.id).over(
#             partition_by=[Register.value],
#             order_by=[Register.id])
#         query = Register.select().order_by(fv)
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."value" FROM "register" AS "t1" '
#             'ORDER BY FIRST_VALUE("t1"."id") '
#             'OVER (PARTITION BY "t1"."value" ORDER BY "t1"."id")'), [])
# 
#     def test_window_extends(self):
#         Tbl = Table('tbl', ('b', 'c'))
#         w1 = Window(partition_by=[Tbl.b], alias='win1')
#         w2 = Window(extends=w1, order_by=[Tbl.c], alias='win2')
#         query = Tbl.select(fn.GROUP_CONCAT(Tbl.c).over(w2)).window(w1, w2)
#         self.assertSQL(query, (
#             'SELECT GROUP_CONCAT("t1"."c") OVER win2 FROM "tbl" AS "t1" '
#             'WINDOW win1 AS (PARTITION BY "t1"."b"), '
#             'win2 AS (win1 ORDER BY "t1"."c")'), [])
# 
#         w1 = Window(partition_by=[Tbl.b], alias='w1')
#         w2 = Window(extends=w1).alias('w2')
#         w3 = Window(extends=w2).alias('w3')
#         w4 = Window(extends=w3, order_by=[Tbl.c]).alias('w4')
#         query = (Tbl
#                  .select(fn.GROUP_CONCAT(Tbl.c).over(w4))
#                  .window(w1, w2, w3, w4))
#         self.assertSQL(query, (
#             'SELECT GROUP_CONCAT("t1"."c") OVER w4 FROM "tbl" AS "t1" '
#             'WINDOW w1 AS (PARTITION BY "t1"."b"), w2 AS (w1), w3 AS (w2), '
#             'w4 AS (w3 ORDER BY "t1"."c")'), [])
# 
#     def test_window_ranged(self):
#         Tbl = Table('tbl', ('a', 'b'))
#         query = (Tbl
#                  .select(Tbl.a, fn.SUM(Tbl.b).over(
#                      order_by=[Tbl.a.desc()],
#                      frame_type=Window.RANGE,
#                      start=Window.preceding(1),
#                      end=Window.following(2)))
#                  .order_by(Tbl.a.asc()))
#         self.assertSQL(query, (
#             'SELECT "t1"."a", SUM("t1"."b") OVER ('
#             'ORDER BY "t1"."a" DESC RANGE BETWEEN 1 PRECEDING AND 2 FOLLOWING)'
#             ' FROM "tbl" AS "t1" ORDER BY "t1"."a" ASC'), [])
# 
#         query = (Tbl
#                  .select(Tbl.a, fn.SUM(Tbl.b).over(
#                      order_by=[Tbl.a],
#                      frame_type=Window.GROUPS,
#                      start=Window.preceding(3),
#                      end=Window.preceding(1))))
#         self.assertSQL(query, (
#             'SELECT "t1"."a", SUM("t1"."b") OVER ('
#             'ORDER BY "t1"."a" GROUPS BETWEEN 3 PRECEDING AND 1 PRECEDING) '
#             'FROM "tbl" AS "t1"'), [])
# 
#         query = (Tbl
#                  .select(Tbl.a, fn.SUM(Tbl.b).over(
#                      order_by=[Tbl.a],
#                      frame_type=Window.GROUPS,
#                      start=Window.following(1),
#                      end=Window.following(5))))
#         self.assertSQL(query, (
#             'SELECT "t1"."a", SUM("t1"."b") OVER ('
#             'ORDER BY "t1"."a" GROUPS BETWEEN 1 FOLLOWING AND 5 FOLLOWING) '
#             'FROM "tbl" AS "t1"'), [])
# 
# 
#     def test_window_frametypes(self):
#         Tbl = Table('tbl', ('b', 'c'))
#         fts = (('as_range', Window.RANGE, 'RANGE'),
#                ('as_rows', Window.ROWS, 'ROWS'),
#                ('as_groups', Window.GROUPS, 'GROUPS'))
#         for method, arg, sql in fts:
#             w = getattr(Window(order_by=[Tbl.b + 1]), method)()
#             self.assertSQL(Tbl.select(fn.SUM(Tbl.c).over(w)).window(w), (
#                 'SELECT SUM("t1"."c") OVER w FROM "tbl" AS "t1" '
#                 'WINDOW w AS (ORDER BY ("t1"."b" + ?) '
#                 '%s UNBOUNDED PRECEDING)') % sql, [1])
# 
#             query = Tbl.select(fn.SUM(Tbl.c)
#                                .over(order_by=[Tbl.b + 1], frame_type=arg))
#             self.assertSQL(query, (
#                 'SELECT SUM("t1"."c") OVER (ORDER BY ("t1"."b" + ?) '
#                 '%s UNBOUNDED PRECEDING) FROM "tbl" AS "t1"') % sql, [1])
# 
#     def test_window_frame_exclusion(self):
#         Tbl = Table('tbl', ('b', 'c'))
#         fts = ((Window.CURRENT_ROW, 'CURRENT ROW'),
#                (Window.TIES, 'TIES'),
#                (Window.NO_OTHERS, 'NO OTHERS'),
#                (Window.GROUP, 'GROUP'))
#         for arg, sql in fts:
#             query = Tbl.select(fn.MAX(Tbl.b).over(
#                 order_by=[Tbl.c],
#                 start=Window.preceding(4),
#                 end=Window.following(),
#                 frame_type=Window.ROWS,
#                 exclude=arg))
#             self.assertSQL(query, (
#                 'SELECT MAX("t1"."b") OVER (ORDER BY "t1"."c" '
#                 'ROWS BETWEEN 4 PRECEDING AND UNBOUNDED FOLLOWING '
#                 'EXCLUDE %s) FROM "tbl" AS "t1"') % sql, [])
# 
#     def test_filter_window(self):
#         # Example derived from sqlite window test 5.1.3.2.
#         Tbl = Table('tbl', ('a', 'c'))
#         win = Window(partition_by=fn.COALESCE(Tbl.a, ''),
#                      frame_type=Window.RANGE,
#                      start=Window.CURRENT_ROW,
#                      end=Window.following(),
#                      exclude=Window.NO_OTHERS)
#         query = (Tbl
#                  .select(fn.SUM(Tbl.c).filter(Tbl.c < 5).over(win),
#                          fn.RANK().over(win),
#                          fn.DENSE_RANK().over(win))
#                  .window(win))
#         self.assertSQL(query, (
#             'SELECT SUM("t1"."c") FILTER (WHERE ("t1"."c" < ?)) OVER w, '
#             'RANK() OVER w, DENSE_RANK() OVER w '
#             'FROM "tbl" AS "t1" '
#             'WINDOW w AS (PARTITION BY COALESCE("t1"."a", ?) '
#             'RANGE BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING '
#             'EXCLUDE NO OTHERS)'), [5, ''])
# 
# 
# class TestValuesList(BaseTestCase):
#     _data = [(1, 'one'), (2, 'two'), (3, 'three')]
# 
#     def test_values_list(self):
#         vl = ValuesList(self._data)
# 
#         query = vl.select(SQL('*'))
#         self.assertSQL(query, (
#             'SELECT * FROM (VALUES (?, ?), (?, ?), (?, ?)) AS "t1"'),
#             [1, 'one', 2, 'two', 3, 'three'])
# 
#     def test_values_list_named_columns(self):
#         vl = ValuesList(self._data).columns('idx', 'name')
#         query = (vl
#                  .select(vl.c.idx, vl.c.name)
#                  .order_by(vl.c.idx))
#         self.assertSQL(query, (
#             'SELECT "t1"."idx", "t1"."name" '
#             'FROM (VALUES (?, ?), (?, ?), (?, ?)) AS "t1"("idx", "name") '
#             'ORDER BY "t1"."idx"'), [1, 'one', 2, 'two', 3, 'three'])
# 
#     def test_named_values_list(self):
#         vl = ValuesList(self._data, ['idx', 'name']).alias('vl')
#         query = (vl
#                  .select(vl.c.idx, vl.c.name)
#                  .order_by(vl.c.idx))
#         self.assertSQL(query, (
#             'SELECT "vl"."idx", "vl"."name" '
#             'FROM (VALUES (?, ?), (?, ?), (?, ?)) AS "vl"("idx", "name") '
#             'ORDER BY "vl"."idx"'), [1, 'one', 2, 'two', 3, 'three'])
# 
#     def test_docs_examples(self):
#         data = [(1, 'first'), (2, 'second')]
#         vl = ValuesList(data, columns=('idx', 'name'))
#         query = (vl
#                  .select(vl.c.idx, vl.c.name)
#                  .order_by(vl.c.idx))
#         self.assertSQL(query, (
#             'SELECT "t1"."idx", "t1"."name" '
#             'FROM (VALUES (?, ?), (?, ?)) AS "t1"("idx", "name") '
#             'ORDER BY "t1"."idx"'), [1, 'first', 2, 'second'])
# 
#         vl = ValuesList([(1, 'first'), (2, 'second')])
#         vl = vl.columns('idx', 'name').alias('v')
#         query = vl.select(vl.c.idx, vl.c.name)
#         self.assertSQL(query, (
#             'SELECT "v"."idx", "v"."name" '
#             'FROM (VALUES (?, ?), (?, ?)) AS "v"("idx", "name")'),
#             [1, 'first', 2, 'second'])
# 
#     def test_join_on_valueslist(self):
#         vl = ValuesList([('huey',), ('zaizee',)], columns=['username'])
#         query = (User
#                  .select(vl.c.username)
#                  .join(vl, on=(User.c.username == vl.c.username))
#                  .order_by(vl.c.username.desc()))
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t2" '
#             'INNER JOIN (VALUES (?), (?)) AS "t1"("username") '
#             'ON ("t2"."username" = "t1"."username") '
#             'ORDER BY "t1"."username" DESC'), ['huey', 'zaizee'])
# 
# 
# class TestCaseFunction(BaseTestCase):
#     def test_case_function(self):
#         NameNum = Table('nn', ('name', 'number'))
# 
#         query = (NameNum
#                  .select(NameNum.name, Case(NameNum.number, (
#                      (1, 'one'),
#                      (2, 'two')), '?').alias('num_str')))
#         self.assertSQL(query, (
#             'SELECT "t1"."name", CASE "t1"."number" '
#             'WHEN ? THEN ? '
#             'WHEN ? THEN ? '
#             'ELSE ? END AS "num_str" '
#             'FROM "nn" AS "t1"'), [1, 'one', 2, 'two', '?'])
# 
#         query = (NameNum
#                  .select(NameNum.name, Case(None, (
#                      (NameNum.number == 1, 'one'),
#                      (NameNum.number == 2, 'two')), '?')))
#         self.assertSQL(query, (
#             'SELECT "t1"."name", CASE '
#             'WHEN ("t1"."number" = ?) THEN ? '
#             'WHEN ("t1"."number" = ?) THEN ? '
#             'ELSE ? END '
#             'FROM "nn" AS "t1"'), [1, 'one', 2, 'two', '?'])
# 
#     def test_case_subquery(self):
#         Name = Table('n', ('id', 'name',))
#         case = Case(None, [(Name.id.in_(Name.select(Name.id)), 1)], 0)
#         q = Name.select(fn.SUM(case))
#         self.assertSQL(q, (
#             'SELECT SUM('
#             'CASE WHEN ("t1"."id" IN (SELECT "t1"."id" FROM "n" AS "t1")) '
#             'THEN ? ELSE ? END) FROM "n" AS "t1"'), [1, 0])
# 
#         case = Case(None, [
#             (Name.id < 5, Name.select(fn.SUM(Name.id))),
#             (Name.id > 5, Name.select(fn.COUNT(Name.name)).distinct())],
#             Name.select(fn.MAX(Name.id)))
#         q = Name.select(Name.name, case.alias('magic'))
#         self.assertSQL(q, (
#             'SELECT "t1"."name", CASE '
#             'WHEN ("t1"."id" < ?) '
#             'THEN (SELECT SUM("t1"."id") FROM "n" AS "t1") '
#             'WHEN ("t1"."id" > ?) '
#             'THEN (SELECT DISTINCT COUNT("t1"."name") FROM "n" AS "t1") '
#             'ELSE (SELECT MAX("t1"."id") FROM "n" AS "t1") END AS "magic" '
#             'FROM "n" AS "t1"'), [5, 5])
# 
# 
# 
# class TestSelectFeatures(BaseTestCase):
#     def test_reselect(self):
#         query = Person.select(Person.name)
#         self.assertSQL(query, 'SELECT "t1"."name" FROM "person" AS "t1"', [])
# 
#         query = query.columns(Person.id, Person.name, Person.dob)
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#             'FROM "person" AS "t1"'), [])
# 
#     def test_distinct_on(self):
#         query = (Note
#                  .select(Person.name, Note.content)
#                  .join(Person, on=(Note.person_id == Person.id))
#                  .order_by(Person.name, Note.content)
#                  .distinct(Person.name))
#         self.assertSQL(query, (
#             'SELECT DISTINCT ON ("t1"."name") '
#             '"t1"."name", "t2"."content" '
#             'FROM "note" AS "t2" '
#             'INNER JOIN "person" AS "t1" ON ("t2"."person_id" = "t1"."id") '
#             'ORDER BY "t1"."name", "t2"."content"'), [])
# 
#         query = (Person
#                  .select(Person.name)
#                  .distinct(Person.name))
#         self.assertSQL(query, (
#             'SELECT DISTINCT ON ("t1"."name") "t1"."name" '
#             'FROM "person" AS "t1"'), [])
# 
#     def test_distinct(self):
#         query = Person.select(Person.name).distinct()
#         self.assertSQL(query,
#                        'SELECT DISTINCT "t1"."name" FROM "person" AS "t1"', [])
# 
#     def test_distinct_count(self):
#         query = Person.select(fn.COUNT(Person.name.distinct()))
#         self.assertSQL(query, (
#             'SELECT COUNT(DISTINCT "t1"."name") FROM "person" AS "t1"'), [])
# 
#     def test_filtered_count(self):
#         filtered_count = (fn.COUNT(Person.name)
#                           .filter(Person.dob < datetime.date(2000, 1, 1)))
#         query = Person.select(fn.COUNT(Person.name), filtered_count)
#         self.assertSQL(query, (
#             'SELECT COUNT("t1"."name"), COUNT("t1"."name") '
#             'FILTER (WHERE ("t1"."dob" < ?)) '
#             'FROM "person" AS "t1"'), [datetime.date(2000, 1, 1)])
# 
#     def test_ordered_aggregate(self):
#         agg = fn.array_agg(Person.name).order_by(Person.id.desc())
#         self.assertSQL(Person.select(agg.alias('names')), (
#             'SELECT array_agg("t1"."name" ORDER BY "t1"."id" DESC) AS "names" '
#             'FROM "person" AS "t1"'), [])
# 
#         agg = fn.string_agg(Person.name, ',').order_by(Person.dob, Person.id)
#         self.assertSQL(Person.select(agg), (
#             'SELECT string_agg("t1"."name", ? ORDER BY "t1"."dob", "t1"."id")'
#             ' FROM "person" AS "t1"'), [','])
# 
#         agg = (fn.string_agg(Person.name.concat('-x'), ',')
#                .order_by(Person.name.desc(), Person.dob.asc()))
#         self.assertSQL(Person.select(agg), (
#             'SELECT string_agg(("t1"."name" || ?), ? ORDER BY "t1"."name" DESC'
#             ', "t1"."dob" ASC) '
#             'FROM "person" AS "t1"'), ['-x', ','])
# 
#         agg = agg.order_by()
#         self.assertSQL(Person.select(agg), (
#             'SELECT string_agg(("t1"."name" || ?), ?) '
#             'FROM "person" AS "t1"'), ['-x', ','])
# 
#     def test_for_update(self):
#         query = (Person
#                  .select()
#                  .where(Person.name == 'charlie')
#                  .for_update())
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#             'FROM "person" AS "t1" '
#             'WHERE ("t1"."name" = ?) '
#             'FOR UPDATE'), ['charlie'], for_update=True)
# 
#         query = query.for_update('FOR SHARE NOWAIT')
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#             'FROM "person" AS "t1" '
#             'WHERE ("t1"."name" = ?) '
#             'FOR SHARE NOWAIT'), ['charlie'], for_update=True)
# 
#     def test_for_update_nested(self):
#         PA = Person.alias('pa')
#         subq = PA.select(PA.id).where(PA.name == 'charlie').for_update()
#         query = (Person
#                  .delete()
#                  .where(Person.id.in_(subq)))
#         self.assertSQL(query, (
#             'DELETE FROM "person" WHERE ("person"."id" IN ('
#             'SELECT "pa"."id" FROM "person" AS "pa" '
#             'WHERE ("pa"."name" = ?) FOR UPDATE))'),
#             ['charlie'],
#             for_update=True)
# 
#     def test_for_update_options(self):
#         query = (Person
#                  .select(Person.id)
#                  .where(Person.name == 'huey')
#                  .for_update(of=Person, nowait=True))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" WHERE ("t1"."name" = ?) '
#             'FOR UPDATE OF "t1" NOWAIT'), ['huey'], for_update=True)
# 
#         # Check default behavior.
#         query = query.for_update()
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" WHERE ("t1"."name" = ?) '
#             'FOR UPDATE'), ['huey'], for_update=True)
# 
#         # Clear flag.
#         query = query.for_update(None)
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" WHERE ("t1"."name" = ?)'),
#             ['huey'])
# 
#         # Old-style is still supported.
#         query = query.for_update('FOR UPDATE NOWAIT')
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" WHERE ("t1"."name" = ?) '
#             'FOR UPDATE NOWAIT'), ['huey'], for_update=True)
# 
#         # Mix of old and new is OK.
#         query = query.for_update('FOR SHARE NOWAIT', of=Person)
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" WHERE ("t1"."name" = ?) '
#             'FOR SHARE OF "t1" NOWAIT'), ['huey'], for_update=True)
# 
#     def test_parentheses(self):
#         query = (Person
#                  .select(fn.MAX(
#                      fn.IFNULL(1, 10) * 151,
#                      fn.IFNULL(None, 10))))
#         self.assertSQL(query, (
#             'SELECT MAX((IFNULL(?, ?) * ?), IFNULL(?, ?)) '
#             'FROM "person" AS "t1"'), [1, 10, 151, None, 10])
# 
#         query = (Person
#                  .select(Person.name)
#                  .where(fn.EXISTS(
#                      User.select(User.c.id).where(
#                          User.c.username == Person.name))))
#         self.assertSQL(query, (
#             'SELECT "t1"."name" FROM "person" AS "t1" '
#             'WHERE EXISTS('
#             'SELECT "t2"."id" FROM "users" AS "t2" '
#             'WHERE ("t2"."username" = "t1"."name"))'), [])
# 
# 
# class TestExpressionSQL(BaseTestCase):
#     def test_parentheses_functions(self):
#         expr = (User.c.income + 100)
#         expr2 = expr * expr
#         query = User.select(fn.sum(expr), fn.avg(expr2))
#         self.assertSQL(query, (
#             'SELECT sum("t1"."income" + ?), '
#             'avg(("t1"."income" + ?) * ("t1"."income" + ?)) '
#             'FROM "users" AS "t1"'), [100, 100, 100])
# 
# 
# #Person = Table('person', ['id', 'name', 'dob'])
# 
# class TestOnConflictSqlite(BaseTestCase):
#     database = SqliteDatabase(None)
# 
#     def test_replace(self):
#         query = Person.insert(name='huey').on_conflict('replace')
#         self.assertSQL(query, (
#             'INSERT OR REPLACE INTO "person" ("name") VALUES (?)'), ['huey'])
# 
#     def test_ignore(self):
#         query = Person.insert(name='huey').on_conflict('ignore')
#         self.assertSQL(query, (
#             'INSERT OR IGNORE INTO "person" ("name") VALUES (?)'), ['huey'])
# 
#     def test_update_not_supported(self):
#         query = Person.insert(name='huey').on_conflict(
#             preserve=(Person.dob,),
#             update={Person.name: Person.name.concat(' (updated)')})
#         with self.assertRaisesCtx(ValueError):
#             self.database.get_sql_context().parse(query)
# 
# 
# class TestOnConflictMySQL(BaseTestCase):
#     database = MySQLDatabase(None)
# 
#     def setUp(self):
#         super(TestOnConflictMySQL, self).setUp()
#         self.database.server_version = None
# 
#     def test_replace(self):
#         query = Person.insert(name='huey').on_conflict('replace')
#         self.assertSQL(query, (
#             'REPLACE INTO "person" ("name") VALUES (?)'), ['huey'])
# 
#     def test_ignore(self):
#         query = Person.insert(name='huey').on_conflict('ignore')
#         self.assertSQL(query, (
#             'INSERT IGNORE INTO "person" ("name") VALUES (?)'), ['huey'])
# 
#     def test_update(self):
#         dob = datetime.date(2010, 1, 1)
#         query = (Person
#                  .insert(name='huey', dob=dob)
#                  .on_conflict(
#                      preserve=(Person.dob,),
#                      update={Person.name: Person.name.concat('-x')}))
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON DUPLICATE KEY '
#             'UPDATE "dob" = VALUES("dob"), "name" = ("name" || ?)'),
#             [dob, 'huey', '-x'])
# 
#         query = (Person
#                  .insert(name='huey', dob=dob)
#                  .on_conflict(preserve='dob'))
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON DUPLICATE KEY '
#             'UPDATE "dob" = VALUES("dob")'), [dob, 'huey'])
# 
#     def test_update_use_value_mariadb(self):
#         # Verify that we use "VALUE" (not "VALUES") for MariaDB 10.3.3.
#         dob = datetime.date(2010, 1, 1)
#         query = (Person
#                  .insert(name='huey', dob=dob)
#                  .on_conflict(preserve=(Person.dob,)))
#         self.database.server_version = (10, 3, 3)
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON DUPLICATE KEY '
#             'UPDATE "dob" = VALUE("dob")'), [dob, 'huey'])
# 
#         self.database.server_version = (10, 3, 2)
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON DUPLICATE KEY '
#             'UPDATE "dob" = VALUES("dob")'), [dob, 'huey'])
# 
#     def test_where_not_supported(self):
#         query = Person.insert(name='huey').on_conflict(
#             preserve=(Person.dob,),
#             where=(Person.name == 'huey'))
#         with self.assertRaisesCtx(ValueError):
#             self.database.get_sql_context().parse(query)
# 
# 
# class TestOnConflictPostgresql(BaseTestCase):
#     database = PostgresqlDatabase(None)
# 
#     def test_ignore(self):
#         query = Person.insert(name='huey').on_conflict('ignore')
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("name") VALUES (?) '
#             'ON CONFLICT DO NOTHING'), ['huey'])
# 
#     def test_conflict_target_required(self):
#         query = Person.insert(name='huey').on_conflict(preserve=(Person.dob,))
#         with self.assertRaisesCtx(ValueError):
#             self.database.get_sql_context().parse(query)
# 
#     def test_conflict_resolution_required(self):
#         query = Person.insert(name='huey').on_conflict(conflict_target='name')
#         with self.assertRaisesCtx(ValueError):
#             self.database.get_sql_context().parse(query)
# 
#     def test_conflict_update_excluded(self):
#         KV = Table('kv', ('key', 'value', 'extra'), _database=self.database)
# 
#         query = (KV.insert(key='k1', value='v1', extra=1)
#                  .on_conflict(conflict_target=(KV.key, KV.value),
#                               update={KV.extra: EXCLUDED.extra + 2},
#                               where=(EXCLUDED.extra < KV.extra)))
#         self.assertSQL(query, (
#             'INSERT INTO "kv" ("extra", "key", "value") VALUES (?, ?, ?) '
#             'ON CONFLICT ("key", "value") DO UPDATE '
#             'SET "extra" = (EXCLUDED."extra" + ?) '
#             'WHERE (EXCLUDED."extra" < "kv"."extra")'), [1, 'k1', 'v1', 2])
# 
#     def test_conflict_target_or_constraint(self):
#         KV = Table('kv', ('key', 'value', 'extra'), _database=self.database)
# 
#         query = (KV.insert(key='k1', value='v1', extra='e1')
#                  .on_conflict(conflict_target=[KV.key, KV.value],
#                               preserve=[KV.extra]))
#         self.assertSQL(query, (
#             'INSERT INTO "kv" ("extra", "key", "value") VALUES (?, ?, ?) '
#             'ON CONFLICT ("key", "value") DO UPDATE '
#             'SET "extra" = EXCLUDED."extra"'), ['e1', 'k1', 'v1'])
# 
#         query = (KV.insert(key='k1', value='v1', extra='e1')
#                  .on_conflict(conflict_constraint='kv_key_value',
#                               preserve=[KV.extra]))
#         self.assertSQL(query, (
#             'INSERT INTO "kv" ("extra", "key", "value") VALUES (?, ?, ?) '
#             'ON CONFLICT ON CONSTRAINT "kv_key_value" DO UPDATE '
#             'SET "extra" = EXCLUDED."extra"'), ['e1', 'k1', 'v1'])
# 
#         query = KV.insert(key='k1', value='v1', extra='e1')
#         self.assertRaises(ValueError, query.on_conflict,
#                           conflict_target=[KV.key, KV.value],
#                           conflict_constraint='kv_key_value')
# 
#     def test_update(self):
#         dob = datetime.date(2010, 1, 1)
#         query = (Person
#                  .insert(name='huey', dob=dob)
#                  .on_conflict(
#                      conflict_target=(Person.name,),
#                      preserve=(Person.dob,),
#                      update={Person.name: Person.name.concat('-x')}))
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON CONFLICT ("name") DO '
#             'UPDATE SET "dob" = EXCLUDED."dob", '
#             '"name" = ("person"."name" || ?)'),
#             [dob, 'huey', '-x'])
# 
#         query = (Person
#                  .insert(name='huey', dob=dob)
#                  .on_conflict(
#                      conflict_target='name',
#                      preserve='dob'))
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("dob", "name") VALUES (?, ?) '
#             'ON CONFLICT ("name") DO '
#             'UPDATE SET "dob" = EXCLUDED."dob"'), [dob, 'huey'])
# 
#         query = (Person
#                  .insert(name='huey')
#                  .on_conflict(
#                      conflict_target=Person.name,
#                      preserve=Person.dob,
#                      update={Person.name: Person.name.concat('-x')},
#                      where=(Person.name != 'zaizee')))
#         self.assertSQL(query, (
#             'INSERT INTO "person" ("name") VALUES (?) '
#             'ON CONFLICT ("name") DO '
#             'UPDATE SET "dob" = EXCLUDED."dob", '
#             '"name" = ("person"."name" || ?) '
#             'WHERE ("person"."name" != ?)'), ['huey', '-x', 'zaizee'])
# 
#     def test_conflict_target_partial_index(self):
#         KVE = Table('kve', ('key', 'value', 'extra'))
#         data = [('k1', 1, 2), ('k2', 2, 3)]
#         columns = [KVE.key, KVE.value, KVE.extra]
# 
#         query = (KVE
#                  .insert(data, columns)
#                  .on_conflict(
#                      conflict_target=(KVE.key, KVE.value),
#                      conflict_where=(KVE.extra > 1),
#                      preserve=(KVE.extra,),
#                      where=(KVE.key != 'kx')))
#         self.assertSQL(query, (
#             'INSERT INTO "kve" ("key", "value", "extra") '
#             'VALUES (?, ?, ?), (?, ?, ?) '
#             'ON CONFLICT ("key", "value") WHERE ("extra" > ?) '
#             'DO UPDATE SET "extra" = EXCLUDED."extra" '
#             'WHERE ("kve"."key" != ?)'),
#             ['k1', 1, 2, 'k2', 2, 3, 1, 'kx'])
# 
# 
# #Person = Table('person', ['id', 'name', 'dob'])
# #Note = Table('note', ['id', 'person_id', 'content'])
# 
# class TestIndex(BaseTestCase):
#     def test_simple_index(self):
#         pidx = Index('person_name', Person, (Person.name,), unique=True)
#         self.assertSQL(pidx, (
#             'CREATE UNIQUE INDEX "person_name" ON "person" ("name")'), [])
# 
#         pidx = pidx.where(Person.dob > datetime.date(1950, 1, 1))
#         self.assertSQL(pidx, (
#             'CREATE UNIQUE INDEX "person_name" ON "person" '
#             '("name") WHERE ("dob" > ?)'), [datetime.date(1950, 1, 1)])
# 
#     def test_advanced_index(self):
#         Article = Table('article')
#         aidx = Index('foo_idx', Article, (
#             Article.c.status,
#             Article.c.timestamp.desc(),
#             fn.SUBSTR(Article.c.title, 1, 1)), safe=True)
#         self.assertSQL(aidx, (
#             'CREATE INDEX IF NOT EXISTS "foo_idx" ON "article" '
#             '("status", "timestamp" DESC, SUBSTR("title", ?, ?))'), [1, 1])
# 
#         aidx = aidx.where(Article.c.flags.bin_and(4) == 4)
#         self.assertSQL(aidx, (
#             'CREATE INDEX IF NOT EXISTS "foo_idx" ON "article" '
#             '("status", "timestamp" DESC, SUBSTR("title", ?, ?)) '
#             'WHERE (("flags" & ?) = ?)'), [1, 1, 4, 4])
# 
#         # Check behavior when value-literals are enabled.
#         self.assertSQL(aidx, (
#             'CREATE INDEX IF NOT EXISTS "foo_idx" ON "article" '
#             '("status", "timestamp" DESC, SUBSTR("title", 1, 1)) '
#             'WHERE (("flags" & 4) = 4)'), [], value_literals=True)
# 
#     def test_str_cols(self):
#         uidx = Index('users_info', User, ('username DESC', 'id'))
#         self.assertSQL(uidx, (
#             'CREATE INDEX "users_info" ON "users" (username DESC, id)'), [])
# 
# 
# class TestSqlToString(BaseTestCase):
#     def _test_sql_to_string(self, _param):
#         class FakeDB(SqliteDatabase):
#             param = _param
# 
#         db = FakeDB(None)
#         T = Table('tbl', ('id', 'val')).bind(db)
# 
#         query = (T.select()
#                  .where((T.val == 'foo') |
#                         (T.val == b'bar') |
#                         (T.val == True) | (T.val == False) |
#                         (T.val == 2) |
#                         (T.val == -3.14) |
#                         (T.val == datetime.datetime(2018, 1, 1)) |
#                         (T.val == datetime.date(2018, 1, 2)) |
#                         T.val.is_null() |
#                         T.val.is_null(False) |
#                         T.val.in_(['aa', 'bb', 'cc'])))
# 
#         self.assertEqual(query_to_string(query), (
#             'SELECT "t1"."id", "t1"."val" FROM "tbl" AS "t1" WHERE ((((((((((('
#             '"t1"."val" = \'foo\') OR '
#             '("t1"."val" = \'bar\')) OR '
#             '("t1"."val" = 1)) OR '
#             '("t1"."val" = 0)) OR '
#             '("t1"."val" = 2)) OR '
#             '("t1"."val" = -3.14)) OR '
#             '("t1"."val" = \'2018-01-01 00:00:00\')) OR '
#             '("t1"."val" = \'2018-01-02\')) OR '
#             '("t1"."val" IS NULL)) OR '
#             '("t1"."val" IS NOT NULL)) OR '
#             '("t1"."val" IN (\'aa\', \'bb\', \'cc\')))'))
# 
#     def test_sql_to_string_qmark(self):
#         self._test_sql_to_string('?')
# 
#     def test_sql_to_string_default(self):
#         self._test_sql_to_string('%s')
#         