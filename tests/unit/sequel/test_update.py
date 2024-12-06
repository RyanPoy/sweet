# class TestUpdateQuery(BaseTestCase):
#     def test_update_query(self):
#         query = (User
#                  .update({
#                      User.c.username: 'nuggie',
#                      User.c.admin: False,
#                      User.c.counter: User.c.counter + 1})
#                  .where(User.c.username == 'nugz'))
#         self.assertSQL(query, (
#             'UPDATE "users" SET '
#             '"admin" = ?, '
#             '"counter" = ("users"."counter" + ?), '
#             '"username" = ? '
#             'WHERE ("users"."username" = ?)'), [False, 1, 'nuggie', 'nugz'])
#
#     def test_update_subquery(self):
#         count = fn.COUNT(Tweet.c.id).alias('ct')
#         subquery = (User
#                     .select(User.c.id, count)
#                     .join(Tweet, on=(Tweet.c.user_id == User.c.id))
#                     .group_by(User.c.id)
#                     .having(count > 100))
#         query = (User
#                  .update({
#                      User.c.muted: True,
#                      User.c.counter: 0})
#                  .where(User.c.id << subquery))
#         self.assertSQL(query, (
#             'UPDATE "users" SET '
#             '"counter" = ?, '
#             '"muted" = ? '
#             'WHERE ("users"."id" IN ('
#             'SELECT "users"."id", COUNT("t1"."id") AS "ct" '
#             'FROM "users" AS "users" '
#             'INNER JOIN "tweets" AS "t1" '
#             'ON ("t1"."user_id" = "users"."id") '
#             'GROUP BY "users"."id" '
#             'HAVING ("ct" > ?)))'), [0, True, 100])
#
#     def test_update_value_subquery(self):
#         subquery = (Tweet
#                     .select(fn.MAX(Tweet.c.id))
#                     .where(Tweet.c.user_id == User.c.id))
#         query = (User
#                  .update({User.c.last_tweet_id: subquery})
#                  .where(User.c.last_tweet_id.is_null(True)))
#         self.assertSQL(query, (
#             'UPDATE "users" SET '
#             '"last_tweet_id" = (SELECT MAX("t1"."id") FROM "tweets" AS "t1" '
#             'WHERE ("t1"."user_id" = "users"."id")) '
#             'WHERE ("users"."last_tweet_id" IS NULL)'), [])
#
#     def test_update_from(self):
#         data = [(1, 'u1-x'), (2, 'u2-x')]
#         vl = ValuesList(data, columns=('id', 'username'), alias='tmp')
#         query = (User
#                  .update(username=vl.c.username)
#                  .from_(vl)
#                  .where(User.c.id == vl.c.id))
#         self.assertSQL(query, (
#             'UPDATE "users" SET "username" = "tmp"."username" '
#             'FROM (VALUES (?, ?), (?, ?)) AS "tmp"("id", "username") '
#             'WHERE ("users"."id" = "tmp"."id")'), [1, 'u1-x', 2, 'u2-x'])
#
#         subq = vl.select(vl.c.id, vl.c.username)
#         query = (User
#                  .update({User.c.username: subq.c.username})
#                  .from_(subq)
#                  .where(User.c.id == subq.c.id))
#         self.assertSQL(query, (
#             'UPDATE "users" SET "username" = "t1"."username" FROM ('
#             'SELECT "tmp"."id", "tmp"."username" '
#             'FROM (VALUES (?, ?), (?, ?)) AS "tmp"("id", "username")) AS "t1" '
#             'WHERE ("users"."id" = "t1"."id")'), [1, 'u1-x', 2, 'u2-x'])
#
#     def test_update_returning(self):
#         query = (User
#                  .update({User.c.is_admin: True})
#                  .where(User.c.username == 'charlie')
#                  .returning(User.c.id))
#         self.assertSQL(query, (
#             'UPDATE "users" SET "is_admin" = ? WHERE ("users"."username" = ?) '
#             'RETURNING "users"."id"'), [True, 'charlie'])
#
#         query = query.returning(User.c.is_admin.alias('new_is_admin'))
#         self.assertSQL(query, (
#             'UPDATE "users" SET "is_admin" = ? WHERE ("users"."username" = ?) '
#             'RETURNING "users"."is_admin" AS "new_is_admin"'),
#             [True, 'charlie'])
#
#