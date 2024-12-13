# class TestSelectQuery(BaseTestCase):
#     def test_select(self):
#         query = (User
#                  .select(User.c.id, User.c.username)
#                  .where(User.c.username == 'foo'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."username" '
#             'FROM "users" AS "t1" '
#             'WHERE ("t1"."username" = ?)'), ['foo'])
#
#         query = (User
#                  .select(User.c['id'], User.c['username'])
#                  .where(User.c['username'] == 'test'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."username" '
#             'FROM "users" AS "t1" '
#             'WHERE ("t1"."username" = ?)'), ['test'])
#
#     def test_select_extend(self):
#         query = User.select(User.c.id, User.c.username)
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."username" FROM "users" AS "t1"'), [])
#
#         query = query.select(User.c.username, User.c.is_admin)
#         self.assertSQL(query, (
#             'SELECT "t1"."username", "t1"."is_admin" FROM "users" AS "t1"'),
#             [])
#
#         query = query.select_extend(User.c.is_active, User.c.id)
#         self.assertSQL(query, (
#             'SELECT "t1"."username", "t1"."is_admin", "t1"."is_active", '
#             '"t1"."id" FROM "users" AS "t1"'), [])
#
#     def test_selected_columns(self):
#         query = (User
#                  .select(User.c.id, User.c.username, fn.COUNT(Tweet.c.id))
#                  .join(Tweet, JOIN.LEFT_OUTER,
#                        on=(User.c.id == Tweet.c.user_id)))
#         # NOTE: because of operator overloads for equality we have to test by
#         # asserting the attributes of the selected cols.
#         c_id, c_username, c_ct = query.selected_columns
#         self.assertEqual(c_id.name, 'id')
#         self.assertTrue(c_id.source is User)
#         self.assertEqual(c_username.name, 'username')
#         self.assertTrue(c_username.source is User)
#         self.assertTrue(isinstance(c_ct, Function))
#         self.assertEqual(c_ct.name, 'COUNT')
#         c_tid, = c_ct.arguments
#         self.assertEqual(c_tid.name, 'id')
#         self.assertTrue(c_tid.source is Tweet)
#
#         query.selected_columns = (User.c.username,)
#         c_username, = query.selected_columns
#         self.assertEqual(c_username.name, 'username')
#         self.assertTrue(c_username.source is User)
#
#     def test_select_explicit_columns(self):
#         query = (Person
#                  .select()
#                  .where(Person.dob < datetime.date(1980, 1, 1)))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#             'FROM "person" AS "t1" '
#             'WHERE ("t1"."dob" < ?)'), [datetime.date(1980, 1, 1)])
#
#     def test_select_in_list_of_values(self):
#         names_vals = [
#             ['charlie', 'huey'],
#             ('charlie', 'huey'),
#             set(('charlie', 'huey')),
#             frozenset(('charlie', 'huey'))]
#
#         for names in names_vals:
#             query = (Person
#                      .select()
#                      .where(Person.name.in_(names)))
#             sql, params = Context().sql(query).query()
#             self.assertEqual(sql, (
#                 'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#                 'FROM "person" AS "t1" '
#                 'WHERE ("t1"."name" IN (?, ?))'))
#             self.assertEqual(sorted(params), ['charlie', 'huey'])
#
#         query = (Person
#                  .select()
#                  .where(Person.id.in_(range(1, 10, 2))))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name", "t1"."dob" '
#             'FROM "person" AS "t1" '
#             'WHERE ("t1"."id" IN (?, ?, ?, ?, ?))'), [1, 3, 5, 7, 9])
#
#     def test_select_subselect_function(self):
#         # For functions whose only argument is a subquery, we do not need to
#         # include additional parentheses -- in fact, some databases will report
#         # a syntax error if we do.
#         exists = fn.EXISTS(Tweet
#                            .select(Tweet.c.id)
#                            .where(Tweet.c.user_id == User.c.id))
#         query = User.select(User.c.username, exists.alias('has_tweet'))
#         self.assertSQL(query, (
#             'SELECT "t1"."username", EXISTS('
#             'SELECT "t2"."id" FROM "tweets" AS "t2" '
#             'WHERE ("t2"."user_id" = "t1"."id")) AS "has_tweet" '
#             'FROM "users" AS "t1"'), [])
#
#         # If the function has more than one argument, we need to wrap the
#         # subquery in parentheses.
#         Stat = Table('stat', ['id', 'val'])
#         SA = Stat.alias('sa')
#         subq = SA.select(fn.SUM(SA.val).alias('val_sum'))
#         query = Stat.select(fn.COALESCE(subq, 0))
#         self.assertSQL(query, (
#             'SELECT COALESCE(('
#             'SELECT SUM("sa"."val") AS "val_sum" FROM "stat" AS "sa"'
#             '), ?) FROM "stat" AS "t1"'), [0])
#
#     def test_subquery_in_select_sql(self):
#         subq = User.select(User.c.id).where(User.c.username == 'huey')
#         query = Tweet.select(Tweet.c.content,
#                              Tweet.c.user_id.in_(subq).alias('is_huey'))
#         self.assertSQL(query, (
#             'SELECT "t1"."content", ("t1"."user_id" IN ('
#             'SELECT "t2"."id" FROM "users" AS "t2" WHERE ("t2"."username" = ?)'
#             ')) AS "is_huey" FROM "tweets" AS "t1"'), ['huey'])
#
#         # If we explicitly specify an alias, it will be included.
#         subq = subq.alias('sq')
#         query = Tweet.select(Tweet.c.content,
#                              Tweet.c.user_id.in_(subq).alias('is_huey'))
#         self.assertSQL(query, (
#             'SELECT "t1"."content", ("t1"."user_id" IN ('
#             'SELECT "t2"."id" FROM "users" AS "t2" WHERE ("t2"."username" = ?)'
#             ') AS "sq") AS "is_huey" FROM "tweets" AS "t1"'), ['huey'])
#
#     def test_subquery_in_select_expression_sql(self):
#         Point = Table('point', ('x', 'y'))
#         PA = Point.alias('pa')
#
#         subq = PA.select(fn.SUM(PA.y).alias('sa')).where(PA.x == Point.x)
#         query = (Point
#                  .select(Point.x, Point.y, subq.alias('sy'))
#                  .order_by(Point.x, Point.y))
#         self.assertSQL(query, (
#             'SELECT "t1"."x", "t1"."y", ('
#             'SELECT SUM("pa"."y") AS "sa" FROM "point" AS "pa" '
#             'WHERE ("pa"."x" = "t1"."x")) AS "sy" '
#             'FROM "point" AS "t1" '
#             'ORDER BY "t1"."x", "t1"."y"'), [])
#
#     def test_star(self):
#         query = User.select(User.__star__)
#         self.assertSQL(query, ('SELECT "t1".* FROM "users" AS "t1"'), [])
#
#         query = (Tweet
#                  .select(Tweet.__star__, User.__star__)
#                  .join(User, on=(Tweet.c.user_id == User.c.id)))
#         self.assertSQL(query, (
#             'SELECT "t1".*, "t2".* '
#             'FROM "tweets" AS "t1" '
#             'INNER JOIN "users" AS "t2" ON ("t1"."user_id" = "t2"."id")'), [])
#
#         query = (Tweet
#                  .select(Tweet.__star__, User.c.id)
#                  .join(User, on=(Tweet.c.user_id == User.c.id)))
#         self.assertSQL(query, (
#             'SELECT "t1".*, "t2"."id" '
#             'FROM "tweets" AS "t1" '
#             'INNER JOIN "users" AS "t2" ON ("t1"."user_id" = "t2"."id")'), [])
#
#     def test_from_clause(self):
#         query = (Note
#                  .select(Note.content, Person.name)
#                  .from_(Note, Person)
#                  .where(Note.person_id == Person.id)
#                  .order_by(Note.id))
#         self.assertSQL(query, (
#             'SELECT "t1"."content", "t2"."name" '
#             'FROM "note" AS "t1", "person" AS "t2" '
#             'WHERE ("t1"."person_id" = "t2"."id") '
#             'ORDER BY "t1"."id"'), [])
#
#     def test_from_query(self):
#         inner = Person.select(Person.name)
#         query = (Person
#                  .select(Person.name)
#                  .from_(inner.alias('i1')))
#         self.assertSQL(query, (
#             'SELECT "t1"."name" '
#             'FROM (SELECT "t1"."name" FROM "person" AS "t1") AS "i1"'), [])
#
#         PA = Person.alias('pa')
#         inner = PA.select(PA.name).alias('i1')
#         query = (Person
#                  .select(inner.c.name)
#                  .from_(inner)
#                  .order_by(inner.c.name))
#         self.assertSQL(query, (
#             'SELECT "i1"."name" '
#             'FROM (SELECT "pa"."name" FROM "person" AS "pa") AS "i1" '
#             'ORDER BY "i1"."name"'), [])
#
#     def test_join_explicit_columns(self):
#         query = (Note
#                  .select(Note.content)
#                  .join(Person, on=(Note.person_id == Person.id))
#                  .where(Person.name == 'charlie')
#                  .order_by(Note.id.desc()))
#         self.assertSQL(query, (
#             'SELECT "t1"."content" '
#             'FROM "note" AS "t1" '
#             'INNER JOIN "person" AS "t2" ON ("t1"."person_id" = "t2"."id") '
#             'WHERE ("t2"."name" = ?) '
#             'ORDER BY "t1"."id" DESC'), ['charlie'])
#
#     def test_multi_join(self):
#         Like = Table('likes')
#         LikeUser = User.alias('lu')
#         query = (Like
#                  .select(Tweet.c.content, User.c.username, LikeUser.c.username)
#                  .join(Tweet, on=(Like.c.tweet_id == Tweet.c.id))
#                  .join(User, on=(Tweet.c.user_id == User.c.id))
#                  .join(LikeUser, on=(Like.c.user_id == LikeUser.c.id))
#                  .where(LikeUser.c.username == 'charlie')
#                  .order_by(Tweet.c.timestamp))
#         self.assertSQL(query, (
#             'SELECT "t1"."content", "t2"."username", "lu"."username" '
#             'FROM "likes" AS "t3" '
#             'INNER JOIN "tweets" AS "t1" ON ("t3"."tweet_id" = "t1"."id") '
#             'INNER JOIN "users" AS "t2" ON ("t1"."user_id" = "t2"."id") '
#             'INNER JOIN "users" AS "lu" ON ("t3"."user_id" = "lu"."id") '
#             'WHERE ("lu"."username" = ?) '
#             'ORDER BY "t1"."timestamp"'), ['charlie'])
#
#     def test_correlated_subquery(self):
#         Employee = Table('employee', ['id', 'name', 'salary', 'dept'])
#         EA = Employee.alias('e2')
#         query = (Employee
#                  .select(Employee.id, Employee.name)
#                  .where(Employee.salary > (EA
#                                            .select(fn.AVG(EA.salary))
#                                            .where(EA.dept == Employee.dept))))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."name" '
#             'FROM "employee" AS "t1" '
#             'WHERE ("t1"."salary" > ('
#             'SELECT AVG("e2"."salary") '
#             'FROM "employee" AS "e2" '
#             'WHERE ("e2"."dept" = "t1"."dept")))'), [])
#
#     def test_multiple_where(self):
#         """Ensure multiple calls to WHERE are AND-ed together."""
#         query = (Person
#                  .select(Person.name)
#                  .where(Person.dob < datetime.date(1980, 1, 1))
#                  .where(Person.dob > datetime.date(1950, 1, 1)))
#         self.assertSQL(query, (
#             'SELECT "t1"."name" '
#             'FROM "person" AS "t1" '
#             'WHERE (("t1"."dob" < ?) AND ("t1"."dob" > ?))'),
#             [datetime.date(1980, 1, 1), datetime.date(1950, 1, 1)])
#
#     def test_orwhere(self):
#         query = (Person
#                  .select(Person.name)
#                  .orwhere(Person.dob > datetime.date(1980, 1, 1))
#                  .orwhere(Person.dob < datetime.date(1950, 1, 1)))
#         self.assertSQL(query, (
#             'SELECT "t1"."name" '
#             'FROM "person" AS "t1" '
#             'WHERE (("t1"."dob" > ?) OR ("t1"."dob" < ?))'),
#             [datetime.date(1980, 1, 1), datetime.date(1950, 1, 1)])
#
#     def test_limit(self):
#         base = User.select(User.c.id)
#         self.assertSQL(base.limit(None), (
#             'SELECT "t1"."id" FROM "users" AS "t1"'), [])
#         self.assertSQL(base.limit(10), (
#             'SELECT "t1"."id" FROM "users" AS "t1" LIMIT ?'), [10])
#         self.assertSQL(base.limit(10).offset(3), (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'LIMIT ? OFFSET ?'), [10, 3])
#         self.assertSQL(base.limit(0), (
#             'SELECT "t1"."id" FROM "users" AS "t1" LIMIT ?'), [0])
#
#         self.assertSQL(base.offset(3), (
#             'SELECT "t1"."id" FROM "users" AS "t1" OFFSET ?'), [3],
#             limit_max=None)
#         # Some databases do not support offset without corresponding LIMIT:
#         self.assertSQL(base.offset(3), (
#             'SELECT "t1"."id" FROM "users" AS "t1" LIMIT ? OFFSET ?'), [-1, 3],
#             limit_max=-1)
#         self.assertSQL(base.limit(0).offset(3), (
#             'SELECT "t1"."id" FROM "users" AS "t1" LIMIT ? OFFSET ?'), [0, 3],
#             limit_max=-1)
#
#     def test_simple_join(self):
#         query = (User
#                  .select(
#                      User.c.id,
#                      User.c.username,
#                      fn.COUNT(Tweet.c.id).alias('ct'))
#                  .join(Tweet, on=(Tweet.c.user_id == User.c.id))
#                  .group_by(User.c.id, User.c.username))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."username", COUNT("t2"."id") AS "ct" '
#             'FROM "users" AS "t1" '
#             'INNER JOIN "tweets" AS "t2" ON ("t2"."user_id" = "t1"."id") '
#             'GROUP BY "t1"."id", "t1"."username"'), [])
#
#     def test_subquery(self):
#         inner = (Tweet
#                  .select(fn.COUNT(Tweet.c.id).alias('ct'))
#                  .where(Tweet.c.user == User.c.id))
#         query = (User
#                  .select(User.c.username, inner.alias('iq'))
#                  .order_by(User.c.username))
#         self.assertSQL(query, (
#             'SELECT "t1"."username", '
#             '(SELECT COUNT("t2"."id") AS "ct" '
#             'FROM "tweets" AS "t2" '
#             'WHERE ("t2"."user" = "t1"."id")) AS "iq" '
#             'FROM "users" AS "t1" ORDER BY "t1"."username"'), [])
#
#     def test_subquery_in_expr(self):
#         Team = Table('team')
#         Challenge = Table('challenge')
#         subq = Team.select(fn.COUNT(Team.c.id) + 1)
#         query = (Challenge
#                  .select((Challenge.c.points / subq).alias('score'))
#                  .order_by(SQL('score')))
#         self.assertSQL(query, (
#             'SELECT ("t1"."points" / ('
#             'SELECT (COUNT("t2"."id") + ?) FROM "team" AS "t2")) AS "score" '
#             'FROM "challenge" AS "t1" ORDER BY score'), [1])
#
#     def test_user_defined_alias(self):
#         UA = User.alias('alt')
#         query = (User
#                  .select(User.c.id, User.c.username, UA.c.nuggz)
#                  .join(UA, on=(User.c.id == UA.c.id))
#                  .order_by(UA.c.nuggz))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."username", "alt"."nuggz" '
#             'FROM "users" AS "t1" '
#             'INNER JOIN "users" AS "alt" ON ("t1"."id" = "alt"."id") '
#             'ORDER BY "alt"."nuggz"'), [])
#
#     def test_simple_cte(self):
#         cte = User.select(User.c.id).cte('user_ids')
#         query = (User
#                  .select(User.c.username)
#                  .where(User.c.id.in_(cte))
#                  .with_cte(cte))
#         self.assertSQL(query, (
#             'WITH "user_ids" AS (SELECT "t1"."id" FROM "users" AS "t1") '
#             'SELECT "t2"."username" FROM "users" AS "t2" '
#             'WHERE ("t2"."id" IN "user_ids")'), [])
#
#     def test_two_ctes(self):
#         c1 = User.select(User.c.id).cte('user_ids')
#         c2 = User.select(User.c.username).cte('user_names')
#         query = (User
#                  .select(c1.c.id, c2.c.username)
#                  .where((c1.c.id == User.c.id) &
#                         (c2.c.username == User.c.username))
#                  .with_cte(c1, c2))
#         self.assertSQL(query, (
#             'WITH "user_ids" AS (SELECT "t1"."id" FROM "users" AS "t1"), '
#             '"user_names" AS (SELECT "t1"."username" FROM "users" AS "t1") '
#             'SELECT "user_ids"."id", "user_names"."username" '
#             'FROM "users" AS "t2" '
#             'WHERE (("user_ids"."id" = "t2"."id") AND '
#             '("user_names"."username" = "t2"."username"))'), [])
#
#     def test_select_from_cte(self):
#         # Use the "select_from()" helper on the CTE object.
#         cte = User.select(User.c.username).cte('user_cte')
#         query = cte.select_from(cte.c.username).order_by(cte.c.username)
#         self.assertSQL(query, (
#             'WITH "user_cte" AS (SELECT "t1"."username" FROM "users" AS "t1") '
#             'SELECT "user_cte"."username" FROM "user_cte" '
#             'ORDER BY "user_cte"."username"'), [])
#
#         # Test selecting from multiple CTEs, which is done manually.
#         c1 = User.select(User.c.username).where(User.c.is_admin == 1).cte('c1')
#         c2 = User.select(User.c.username).where(User.c.is_staff == 1).cte('c2')
#         query = (Select((c1, c2), (c1.c.username, c2.c.username))
#                  .with_cte(c1, c2))
#         self.assertSQL(query, (
#             'WITH "c1" AS ('
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'WHERE ("t1"."is_admin" = ?)), '
#             '"c2" AS ('
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'WHERE ("t1"."is_staff" = ?)) '
#             'SELECT "c1"."username", "c2"."username" FROM "c1", "c2"'), [1, 1])
#
#     def test_materialize_cte(self):
#         cases = (
#             (True, 'MATERIALIZED '),
#             (False, 'NOT MATERIALIZED '),
#             (None, ''))
#         for materialized, clause in cases:
#             cte = (User
#                    .select(User.c.id)
#                    .cte('user_ids', materialized=materialized))
#             query = cte.select_from(cte.c.id).where(cte.c.id < 10)
#             self.assertSQL(query, (
#                 'WITH "user_ids" AS %s('
#                 'SELECT "t1"."id" FROM "users" AS "t1") '
#                 'SELECT "user_ids"."id" FROM "user_ids" '
#                 'WHERE ("user_ids"."id" < ?)') % clause, [10])
#
#     def test_fibonacci_cte(self):
#         q1 = Select(columns=(
#             Value(1).alias('n'),
#             Value(0).alias('fib_n'),
#             Value(1).alias('next_fib_n'))).cte('fibonacci', recursive=True)
#         n = (q1.c.n + 1).alias('n')
#         rterm = Select(columns=(
#             n,
#             q1.c.next_fib_n,
#             q1.c.fib_n + q1.c.next_fib_n)).from_(q1).where(n < 10)
#
#         cases = (
#             (q1.union_all, 'UNION ALL'),
#             (q1.union, 'UNION'))
#         for method, clause in cases:
#             cte = method(rterm)
#             query = cte.select_from(cte.c.n, cte.c.fib_n)
#             self.assertSQL(query, (
#                 'WITH RECURSIVE "fibonacci" AS ('
#                 'SELECT ? AS "n", ? AS "fib_n", ? AS "next_fib_n" '
#                 '%s '
#                 'SELECT ("fibonacci"."n" + ?) AS "n", "fibonacci"."next_fib_n", '
#                 '("fibonacci"."fib_n" + "fibonacci"."next_fib_n") '
#                 'FROM "fibonacci" '
#                 'WHERE ("n" < ?)) '
#                 'SELECT "fibonacci"."n", "fibonacci"."fib_n" '
#                 'FROM "fibonacci"' % clause), [1, 0, 1, 1, 10])
#
#     def test_cte_with_count(self):
#         cte = User.select(User.c.id).cte('user_ids')
#         query = (User
#                  .select(User.c.username)
#                  .join(cte, on=(User.c.id == cte.c.id))
#                  .with_cte(cte))
#         count = Select([query], [fn.COUNT(SQL('1'))])
#         self.assertSQL(count, (
#             'SELECT COUNT(1) FROM ('
#             'WITH "user_ids" AS (SELECT "t1"."id" FROM "users" AS "t1") '
#             'SELECT "t2"."username" FROM "users" AS "t2" '
#             'INNER JOIN "user_ids" ON ("t2"."id" = "user_ids"."id")) '
#             'AS "t3"'), [])
#
#     def test_cte_subquery_in_expression(self):
#         Order = Table('order', ('id', 'description'))
#         Item = Table('item', ('id', 'order_id', 'description'))
#
#         cte = Order.select(fn.MAX(Order.id).alias('max_id')).cte('max_order')
#         qexpr = (Order
#                  .select(Order.id)
#                  .join(cte, on=(Order.id == cte.c.max_id))
#                  .with_cte(cte))
#         query = (Item
#                  .select(Item.id, Item.order_id, Item.description)
#                  .where(Item.order_id.in_(qexpr)))
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."order_id", "t1"."description" '
#             'FROM "item" AS "t1" '
#             'WHERE ("t1"."order_id" IN ('
#             'WITH "max_order" AS ('
#             'SELECT MAX("t2"."id") AS "max_id" FROM "order" AS "t2") '
#             'SELECT "t3"."id" '
#             'FROM "order" AS "t3" '
#             'INNER JOIN "max_order" '
#             'ON ("t3"."id" = "max_order"."max_id")))'), [])
#
#     def test_multi_update_cte(self):
#         data = [(i, 'u%sx' % i) for i in range(1, 3)]
#         vl = ValuesList(data)
#         cte = vl.select().cte('uv', columns=('id', 'username'))
#         subq = cte.select(cte.c.username).where(cte.c.id == User.c.id)
#         query = (User
#                  .update(username=subq)
#                  .where(User.c.id.in_(cte.select(cte.c.id)))
#                  .with_cte(cte))
#         self.assertSQL(query, (
#             'WITH "uv" ("id", "username") AS ('
#             'SELECT * FROM (VALUES (?, ?), (?, ?)) AS "t1") '
#             'UPDATE "users" SET "username" = ('
#             'SELECT "uv"."username" FROM "uv" '
#             'WHERE ("uv"."id" = "users"."id")) '
#             'WHERE ("users"."id" IN (SELECT "uv"."id" FROM "uv"))'),
#             [1, 'u1x', 2, 'u2x'])
#
#     def test_data_modifying_cte_delete(self):
#         Product = Table('products', ('id', 'name', 'timestamp'))
#         Archive = Table('archive', ('id', 'name', 'timestamp'))
#
#         query = (Product.delete()
#                  .where(Product.timestamp < datetime.date(2022, 1, 1))
#                  .returning(Product.id, Product.name, Product.timestamp))
#         cte = query.cte('moved_rows')
#
#         src = Select((cte,), (cte.c.id, cte.c.name, cte.c.timestamp))
#         iq = (Archive
#               .insert(src, (Archive.id, Archive.name, Archive.timestamp))
#               .with_cte(cte))
#         self.assertSQL(iq, (
#             'WITH "moved_rows" AS ('
#             'DELETE FROM "products" WHERE ("products"."timestamp" < ?) '
#             'RETURNING "products"."id", "products"."name", '
#             '"products"."timestamp") '
#             'INSERT INTO "archive" ("id", "name", "timestamp") '
#             'SELECT "moved_rows"."id", "moved_rows"."name", '
#             '"moved_rows"."timestamp" FROM "moved_rows"'),
#             [datetime.date(2022, 1, 1)])
#
#         Part = Table('parts', ('id', 'part', 'sub_part'))
#         base = (Part
#                 .select(Part.sub_part, Part.part)
#                 .where(Part.part == 'p')
#                 .cte('included_parts', recursive=True,
#                      columns=('sub_part', 'part')))
#         PA = Part.alias('p')
#         recursive = (PA
#                      .select(PA.sub_part, PA.part)
#                      .join(base, on=(PA.part == base.c.sub_part)))
#         cte = base.union_all(recursive)
#
#         sq = Select((cte,), (cte.c.part,))
#         query = (Part.delete()
#                  .where(Part.part.in_(sq))
#                  .with_cte(cte))
#         self.assertSQL(query, (
#             'WITH RECURSIVE "included_parts" ("sub_part", "part") AS ('
#             'SELECT "t1"."sub_part", "t1"."part" FROM "parts" AS "t1" '
#             'WHERE ("t1"."part" = ?) '
#             'UNION ALL '
#             'SELECT "p"."sub_part", "p"."part" '
#             'FROM "parts" AS "p" '
#             'INNER JOIN "included_parts" '
#             'ON ("p"."part" = "included_parts"."sub_part")) '
#             'DELETE FROM "parts" '
#             'WHERE ("parts"."part" IN ('
#             'SELECT "included_parts"."part" FROM "included_parts"))'), ['p'])
#
#     def test_data_modifying_cte_update(self):
#         Product = Table('products', ('id', 'name', 'price'))
#         Archive = Table('archive', ('id', 'name', 'price'))
#
#         query = (Product
#                  .update(price=Product.price * 1.05)
#                  .returning(Product.id, Product.name, Product.price))
#         cte = query.cte('t')
#
#         sq = cte.select_from(cte.c.id, cte.c.name, cte.c.price)
#         self.assertSQL(sq, (
#             'WITH "t" AS ('
#             'UPDATE "products" SET "price" = ("products"."price" * ?) '
#             'RETURNING "products"."id", "products"."name", "products"."price")'
#             ' SELECT "t"."id", "t"."name", "t"."price" FROM "t"'), [1.05])
#
#         sq = Select((cte,), (cte.c.id, cte.c.price))
#         uq = (Archive
#               .update(price=sq.c.price)
#               .from_(sq)
#               .where(Archive.id == sq.c.id)
#               .with_cte(cte))
#         self.assertSQL(uq, (
#             'WITH "t" AS ('
#             'UPDATE "products" SET "price" = ("products"."price" * ?) '
#             'RETURNING "products"."id", "products"."name", "products"."price")'
#             ' UPDATE "archive" SET "price" = "t1"."price"'
#             ' FROM (SELECT "t"."id", "t"."price" FROM "t") AS "t1"'
#             ' WHERE ("archive"."id" = "t1"."id")'), [1.05])
#
#     def test_data_modifying_cte_insert(self):
#         Product = Table('products', ('id', 'name', 'price'))
#         Archive = Table('archive', ('id', 'name', 'price'))
#
#         query = (Product
#                  .insert({'name': 'p1', 'price': 10})
#                  .returning(Product.id, Product.name, Product.price))
#         cte = query.cte('t')
#
#         sq = cte.select_from(cte.c.id, cte.c.name, cte.c.price)
#         self.assertSQL(sq, (
#             'WITH "t" AS ('
#             'INSERT INTO "products" ("name", "price") VALUES (?, ?) '
#             'RETURNING "products"."id", "products"."name", "products"."price")'
#             ' SELECT "t"."id", "t"."name", "t"."price" FROM "t"'),
#             ['p1', 10])
#
#         sq = Select((cte,), (cte.c.id, cte.c.name, cte.c.price))
#         iq = (Archive
#               .insert(sq, (sq.c.id, sq.c.name, sq.c.price))
#               .with_cte(cte))
#         self.assertSQL(iq, (
#             'WITH "t" AS ('
#             'INSERT INTO "products" ("name", "price") VALUES (?, ?) '
#             'RETURNING "products"."id", "products"."name", "products"."price")'
#             ' INSERT INTO "archive" ("id", "name", "price")'
#             ' SELECT "t"."id", "t"."name", "t"."price" FROM "t"'), ['p1', 10])
#
#     def test_complex_select(self):
#         Order = Table('orders', columns=(
#             'region',
#             'amount',
#             'product',
#             'quantity'))
#
#         regional_sales = (Order
#                           .select(
#                               Order.region,
#                               fn.SUM(Order.amount).alias('total_sales'))
#                           .group_by(Order.region)
#                           .cte('regional_sales'))
#
#         top_regions = (regional_sales
#                        .select(regional_sales.c.region)
#                        .where(regional_sales.c.total_sales > (
#                            regional_sales.select(
#                                fn.SUM(regional_sales.c.total_sales) / 10)))
#                        .cte('top_regions'))
#
#         query = (Order
#                  .select(
#                      Order.region,
#                      Order.product,
#                      fn.SUM(Order.quantity).alias('product_units'),
#                      fn.SUM(Order.amount).alias('product_sales'))
#                  .where(
#                      Order.region << top_regions.select(top_regions.c.region))
#                  .group_by(Order.region, Order.product)
#                  .with_cte(regional_sales, top_regions))
#
#         self.assertSQL(query, (
#             'WITH "regional_sales" AS ('
#             'SELECT "t1"."region", SUM("t1"."amount") AS "total_sales" '
#             'FROM "orders" AS "t1" '
#             'GROUP BY "t1"."region"'
#             '), '
#             '"top_regions" AS ('
#             'SELECT "regional_sales"."region" '
#             'FROM "regional_sales" '
#             'WHERE ("regional_sales"."total_sales" > '
#             '(SELECT (SUM("regional_sales"."total_sales") / ?) '
#             'FROM "regional_sales"))'
#             ') '
#             'SELECT "t2"."region", "t2"."product", '
#             'SUM("t2"."quantity") AS "product_units", '
#             'SUM("t2"."amount") AS "product_sales" '
#             'FROM "orders" AS "t2" '
#             'WHERE ('
#             '"t2"."region" IN ('
#             'SELECT "top_regions"."region" '
#             'FROM "top_regions")'
#             ') GROUP BY "t2"."region", "t2"."product"'), [10])
#
#     def test_compound_select(self):
#         lhs = User.select(User.c.id).where(User.c.username == 'charlie')
#         rhs = User.select(User.c.username).where(User.c.admin == True)
#         q2 = (lhs | rhs)
#         UA = User.alias('U2')
#         q3 = q2 | UA.select(UA.c.id).where(UA.c.superuser == False)
#
#         self.assertSQL(q3, (
#             'SELECT "t1"."id" '
#             'FROM "users" AS "t1" '
#             'WHERE ("t1"."username" = ?) '
#             'UNION '
#             'SELECT "t2"."username" '
#             'FROM "users" AS "t2" '
#             'WHERE ("t2"."admin" = ?) '
#             'UNION '
#             'SELECT "U2"."id" '
#             'FROM "users" AS "U2" '
#             'WHERE ("U2"."superuser" = ?)'), ['charlie', True, False])
#
#     def test_compound_operations(self):
#         admin = (User
#                  .select(User.c.username, Value('admin').alias('role'))
#                  .where(User.c.is_admin == True))
#         editors = (User
#                    .select(User.c.username, Value('editor').alias('role'))
#                    .where(User.c.is_editor == True))
#
#         union = admin.union(editors)
#         self.assertSQL(union, (
#             'SELECT "t1"."username", ? AS "role" '
#             'FROM "users" AS "t1" '
#             'WHERE ("t1"."is_admin" = ?) '
#             'UNION '
#             'SELECT "t2"."username", ? AS "role" '
#             'FROM "users" AS "t2" '
#             'WHERE ("t2"."is_editor" = ?)'), ['admin', 1, 'editor', 1])
#
#         xcept = editors.except_(admin)
#         self.assertSQL(xcept, (
#             'SELECT "t1"."username", ? AS "role" '
#             'FROM "users" AS "t1" '
#             'WHERE ("t1"."is_editor" = ?) '
#             'EXCEPT '
#             'SELECT "t2"."username", ? AS "role" '
#             'FROM "users" AS "t2" '
#             'WHERE ("t2"."is_admin" = ?)'), ['editor', 1, 'admin', 1])
#
#     def test_compound_parentheses_handling(self):
#         admin = (User
#                  .select(User.c.username, Value('admin').alias('role'))
#                  .where(User.c.is_admin == True)
#                  .order_by(User.c.id.desc())
#                  .limit(3))
#         editors = (User
#                    .select(User.c.username, Value('editor').alias('role'))
#                    .where(User.c.is_editor == True)
#                    .order_by(User.c.id.desc())
#                    .limit(5))
#
#         self.assertSQL((admin | editors), (
#             '(SELECT "t1"."username", ? AS "role" FROM "users" AS "t1" '
#             'WHERE ("t1"."is_admin" = ?) ORDER BY "t1"."id" DESC LIMIT ?) '
#             'UNION '
#             '(SELECT "t2"."username", ? AS "role" FROM "users" AS "t2" '
#             'WHERE ("t2"."is_editor" = ?) ORDER BY "t2"."id" DESC LIMIT ?)'),
#             ['admin', 1, 3, 'editor', 1, 5], compound_select_parentheses=True)
#
#         Reg = Table('register', ('value',))
#         lhs = Reg.select().where(Reg.value < 2)
#         rhs = Reg.select().where(Reg.value > 7)
#         compound = lhs | rhs
#
#         for csq_setting in (1, 2):
#             self.assertSQL(compound, (
#                 '(SELECT "t1"."value" FROM "register" AS "t1" '
#                 'WHERE ("t1"."value" < ?)) '
#                 'UNION '
#                 '(SELECT "t2"."value" FROM "register" AS "t2" '
#                 'WHERE ("t2"."value" > ?))'),
#                 [2, 7], compound_select_parentheses=csq_setting)
#
#         rhs2 = Reg.select().where(Reg.value == 5)
#         c2 = compound | rhs2
#
#         # CSQ = always, we get nested parentheses.
#         self.assertSQL(c2, (
#             '((SELECT "t1"."value" FROM "register" AS "t1" '
#             'WHERE ("t1"."value" < ?)) '
#             'UNION '
#             '(SELECT "t2"."value" FROM "register" AS "t2" '
#             'WHERE ("t2"."value" > ?))) '
#             'UNION '
#             '(SELECT "t2"."value" FROM "register" AS "t2" '
#             'WHERE ("t2"."value" = ?))'),
#             [2, 7, 5], compound_select_parentheses=1)  # Always.
#
#         # CSQ = unnested, no nesting but all individual queries have parens.
#         self.assertSQL(c2, (
#             '(SELECT "t1"."value" FROM "register" AS "t1" '
#             'WHERE ("t1"."value" < ?)) '
#             'UNION '
#             '(SELECT "t2"."value" FROM "register" AS "t2" '
#             'WHERE ("t2"."value" > ?)) '
#             'UNION '
#             '(SELECT "t2"."value" FROM "register" AS "t2" '
#             'WHERE ("t2"."value" = ?))'),
#             [2, 7, 5], compound_select_parentheses=2)  # Un-nested.
#
#     def test_compound_select_order_limit(self):
#         A = Table('a', ('col_a',))
#         B = Table('b', ('col_b',))
#         C = Table('c', ('col_c',))
#         q1 = A.select(A.col_a.alias('foo'))
#         q2 = B.select(B.col_b.alias('foo'))
#         q3 = C.select(C.col_c.alias('foo'))
#         qc = (q1 | q2 | q3)
#         qc = qc.order_by(qc.c.foo.desc()).limit(3)
#
#         self.assertSQL(qc, (
#             'SELECT "t1"."col_a" AS "foo" FROM "a" AS "t1" UNION '
#             'SELECT "t2"."col_b" AS "foo" FROM "b" AS "t2" UNION '
#             'SELECT "t3"."col_c" AS "foo" FROM "c" AS "t3" '
#             'ORDER BY "foo" DESC LIMIT ?'), [3])
#
#         self.assertSQL(qc, (
#             '((SELECT "t1"."col_a" AS "foo" FROM "a" AS "t1") UNION '
#             '(SELECT "t2"."col_b" AS "foo" FROM "b" AS "t2")) UNION '
#             '(SELECT "t3"."col_c" AS "foo" FROM "c" AS "t3") '
#             'ORDER BY "foo" DESC LIMIT ?'),
#             [3], compound_select_parentheses=1)
#
#     def test_compound_select_as_subquery(self):
#         A = Table('a', ('col_a',))
#         B = Table('b', ('col_b',))
#         q1 = A.select(A.col_a.alias('foo'))
#         q2 = B.select(B.col_b.alias('foo'))
#         union = q1 | q2
#
#         # Create an outer query and do grouping.
#         outer = (union
#                  .select_from(union.c.foo, fn.COUNT(union.c.foo).alias('ct'))
#                  .group_by(union.c.foo))
#         self.assertSQL(outer, (
#             'SELECT "t1"."foo", COUNT("t1"."foo") AS "ct" FROM ('
#             'SELECT "t2"."col_a" AS "foo" FROM "a" AS "t2" UNION '
#             'SELECT "t3"."col_b" AS "foo" FROM "b" AS "t3") AS "t1" '
#             'GROUP BY "t1"."foo"'), [])
#
#     def test_join_on_query(self):
#         inner = User.select(User.c.id).alias('j1')
#         query = (Tweet
#                  .select(Tweet.c.content)
#                  .join(inner, on=(Tweet.c.user_id == inner.c.id)))
#         self.assertSQL(query, (
#             'SELECT "t1"."content" FROM "tweets" AS "t1" '
#             'INNER JOIN (SELECT "t2"."id" FROM "users" AS "t2") AS "j1" '
#             'ON ("t1"."user_id" = "j1"."id")'), [])
#
#     def test_join_on_misc(self):
#         cond = fn.Magic(Person.id, Note.id).alias('magic')
#         query = Person.select(Person.id).join(Note, on=cond)
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "person" AS "t1" '
#             'INNER JOIN "note" AS "t2" '
#             'ON Magic("t1"."id", "t2"."id") AS "magic"'), [])
#
#     def test_all_clauses(self):
#         count = fn.COUNT(Tweet.c.id).alias('ct')
#         query = (User
#                  .select(User.c.username, count)
#                  .join(Tweet, JOIN.LEFT_OUTER,
#                        on=(User.c.id == Tweet.c.user_id))
#                  .where(User.c.is_admin == 1)
#                  .group_by(User.c.username)
#                  .having(count > 10)
#                  .order_by(count.desc()))
#         self.assertSQL(query, (
#             'SELECT "t1"."username", COUNT("t2"."id") AS "ct" '
#             'FROM "users" AS "t1" '
#             'LEFT OUTER JOIN "tweets" AS "t2" '
#             'ON ("t1"."id" = "t2"."user_id") '
#             'WHERE ("t1"."is_admin" = ?) '
#             'GROUP BY "t1"."username" '
#             'HAVING ("ct" > ?) '
#             'ORDER BY "ct" DESC'), [1, 10])
#
#     def test_order_by_collate(self):
#         query = (User
#                  .select(User.c.username)
#                  .order_by(User.c.username.asc(collation='binary')))
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'ORDER BY "t1"."username" ASC COLLATE binary'), [])
#
#     def test_order_by_nulls(self):
#         query = (User
#                  .select(User.c.username)
#                  .order_by(User.c.ts.desc(nulls='LAST')))
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'ORDER BY "t1"."ts" DESC NULLS LAST'), [], nulls_ordering=True)
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'ORDER BY CASE WHEN ("t1"."ts" IS NULL) THEN ? ELSE ? END, '
#             '"t1"."ts" DESC'), [1, 0], nulls_ordering=False)
#
#         query = (User
#                  .select(User.c.username)
#                  .order_by(User.c.ts.desc(nulls='first')))
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'ORDER BY "t1"."ts" DESC NULLS first'), [], nulls_ordering=True)
#         self.assertSQL(query, (
#             'SELECT "t1"."username" FROM "users" AS "t1" '
#             'ORDER BY CASE WHEN ("t1"."ts" IS NULL) THEN ? ELSE ? END, '
#             '"t1"."ts" DESC'), [0, 1], nulls_ordering=False)
#
#     def test_in_value_representation(self):
#         query = (User
#                  .select(User.c.id)
#                  .where(User.c.username.in_(['foo', 'bar', 'baz'])))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE ("t1"."username" IN (?, ?, ?))'), ['foo', 'bar', 'baz'])
#
#     def test_tuple_comparison(self):
#         name_dob = Tuple(Person.name, Person.dob)
#         query = (Person
#                  .select(Person.id)
#                  .where(name_dob == ('foo', '2017-01-01')))
#         expected = ('SELECT "t1"."id" FROM "person" AS "t1" '
#                     'WHERE (("t1"."name", "t1"."dob") = (?, ?))')
#         self.assertSQL(query, expected, ['foo', '2017-01-01'])
#
#         # Also works specifying rhs values as Tuple().
#         query = (Person
#                  .select(Person.id)
#                  .where(name_dob == Tuple('foo', '2017-01-01')))
#         self.assertSQL(query, expected, ['foo', '2017-01-01'])
#
#     def test_tuple_comparison_subquery(self):
#         PA = Person.alias('pa')
#         subquery = (PA
#                     .select(PA.name, PA.id)
#                     .where(PA.name != 'huey'))
#
#         query = (Person
#                  .select(Person.name)
#                  .where(Tuple(Person.name, Person.id).in_(subquery)))
#         self.assertSQL(query, (
#             'SELECT "t1"."name" FROM "person" AS "t1" '
#             'WHERE (("t1"."name", "t1"."id") IN ('
#             'SELECT "pa"."name", "pa"."id" FROM "person" AS "pa" '
#             'WHERE ("pa"."name" != ?)))'), ['huey'])
#
#     def test_empty_in(self):
#         query = User.select(User.c.id).where(User.c.username.in_([]))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE (0 = 1)'), [])
#
#         query = User.select(User.c.id).where(User.c.username.not_in([]))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE (1 = 1)'), [])
#
#     def test_add_custom_op(self):
#         def mod(lhs, rhs):
#             return Expression(lhs, '%', rhs)
#
#         Stat = Table('stats')
#         query = (Stat
#                  .select(fn.COUNT(Stat.c.id))
#                  .where(mod(Stat.c.index, 10) == 0))
#         self.assertSQL(query, (
#             'SELECT COUNT("t1"."id") FROM "stats" AS "t1" '
#             'WHERE (("t1"."index" % ?) = ?)'), [10, 0])
#
#     def test_where_convert_to_is_null(self):
#         Note = Table('notes', ('id', 'content', 'user_id'))
#         query = Note.select().where(Note.user_id == None)
#         self.assertSQL(query, (
#             'SELECT "t1"."id", "t1"."content", "t1"."user_id" '
#             'FROM "notes" AS "t1" WHERE ("t1"."user_id" IS NULL)'), [])
#
#     def test_like_escape(self):
#         T = Table('tbl', ('key',))
#         def assertLike(expr, expected):
#             query = T.select().where(expr)
#             sql, params = __sql__(T.select().where(expr))
#             match_obj = re.search(r'\("t1"."key" (ILIKE[^\)]+)\)', sql)
#             if match_obj is None:
#                 raise AssertionError('LIKE expression not found in query.')
#             like, = match_obj.groups()
#             self.assertEqual((like, params), expected)
#
#         cases = (
#             (T.key.contains('base'), ('ILIKE ?', ['%base%'])),
#             (T.key.contains('x_y'), ("ILIKE ? ESCAPE ?", ['%x\\_y%', '\\'])),
#             (T.key.contains('__y'), ("ILIKE ? ESCAPE ?", ['%\\_\\_y%', '\\'])),
#             (T.key.contains('%'), ("ILIKE ? ESCAPE ?", ['%\\%%', '\\'])),
#             (T.key.contains('_%'), ("ILIKE ? ESCAPE ?", ['%\\_\\%%', '\\'])),
#             (T.key.startswith('base'), ("ILIKE ?", ['base%'])),
#             (T.key.startswith('x_y'), ("ILIKE ? ESCAPE ?", ['x\\_y%', '\\'])),
#             (T.key.startswith('x%'), ("ILIKE ? ESCAPE ?", ['x\\%%', '\\'])),
#             (T.key.startswith('_%'), ("ILIKE ? ESCAPE ?", ['\\_\\%%', '\\'])),
#             (T.key.endswith('base'), ("ILIKE ?", ['%base'])),
#             (T.key.endswith('x_y'), ("ILIKE ? ESCAPE ?", ['%x\\_y', '\\'])),
#             (T.key.endswith('x%'), ("ILIKE ? ESCAPE ?", ['%x\\%', '\\'])),
#             (T.key.endswith('_%'), ("ILIKE ? ESCAPE ?", ['%\\_\\%', '\\'])),
#         )
#
#         for expr, expected in cases:
#             assertLike(expr, expected)
#
#     def test_like_expr(self):
#         query = User.select(User.c.id).where(User.c.username.like('%foo%'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE ("t1"."username" LIKE ?)'), ['%foo%'])
#
#         query = User.select(User.c.id).where(User.c.username.ilike('%foo%'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE ("t1"."username" ILIKE ?)'), ['%foo%'])
#
#     def test_field_ops(self):
#         query = User.select(User.c.id).where(User.c.username.regexp('[a-z]+'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE ("t1"."username" REGEXP ?)'), ['[a-z]+'])
#
#         query = User.select(User.c.id).where(User.c.username.contains('abc'))
#         self.assertSQL(query, (
#             'SELECT "t1"."id" FROM "users" AS "t1" '
#             'WHERE ("t1"."username" ILIKE ?)'), ['%abc%'])
#