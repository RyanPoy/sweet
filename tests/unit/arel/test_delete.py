# class TestDeleteQuery(BaseTestCase):
#     def test_delete_query(self):
#         query = (User
#                  .delete()
#                  .where(User.c.username != 'charlie')
#                  .limit(3))
#         self.assertSQL(query, (
#             'DELETE FROM "users" WHERE ("users"."username" != ?) LIMIT ?'),
#             ['charlie', 3])
#
#     def test_delete_subquery(self):
#         count = fn.COUNT(Tweet.c.id).alias('ct')
#         subquery = (User
#                     .select(User.c.id, count)
#                     .join(Tweet, on=(Tweet.c.user_id == User.c.id))
#                     .group_by(User.c.id)
#                     .having(count > 100))
#         query = (User
#                  .delete()
#                  .where(User.c.id << subquery))
#         self.assertSQL(query, (
#             'DELETE FROM "users" '
#             'WHERE ("users"."id" IN ('
#             'SELECT "users"."id", COUNT("t1"."id") AS "ct" '
#             'FROM "users" AS "users" '
#             'INNER JOIN "tweets" AS "t1" ON ("t1"."user_id" = "users"."id") '
#             'GROUP BY "users"."id" '
#             'HAVING ("ct" > ?)))'), [100])
#
#     def test_delete_cte(self):
#         cte = (User
#                .select(User.c.id)
#                .where(User.c.admin == True)
#                .cte('u'))
#         query = (User
#                  .delete()
#                  .where(User.c.id << cte.select(cte.c.id))
#                  .with_cte(cte))
#         self.assertSQL(query, (
#             'WITH "u" AS '
#             '(SELECT "t1"."id" FROM "users" AS "t1" WHERE ("t1"."admin" = ?)) '
#             'DELETE FROM "users" '
#             'WHERE ("users"."id" IN (SELECT "u"."id" FROM "u"))'), [True])
#
#     def test_delete_returning(self):
#         query = (User
#                  .delete()
#                  .where(User.c.id > 2)
#                  .returning(User.c.username))
#         self.assertSQL(query, (
#             'DELETE FROM "users" '
#             'WHERE ("users"."id" > ?) '
#             'RETURNING "users"."username"'), [2])
#
#         query = query.returning(User.c.id, User.c.username, SQL('1'))
#         self.assertSQL(query, (
#             'DELETE FROM "users" '
#             'WHERE ("users"."id" > ?) '
#             'RETURNING "users"."id", "users"."username", 1'), [2])
#
#         query = query.returning(User.c.id.alias('old_id'))
#         self.assertSQL(query, (
#             'DELETE FROM "users" '
#             'WHERE ("users"."id" > ?) '
#             'RETURNING "users"."id" AS "old_id"'), [2])
#
#